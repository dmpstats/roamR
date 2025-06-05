#' Run the DisNBS model
#'
#'
#' @param waypnts_res the distance between waypoints defining the movement path
#'   of simulated agents. Either a `<numeric>` value, expressed in meters, or an
#'   `<units>` object with a valid length units.
#'
#' @include simulate_agent_disnbs.R
#'
#' @export
run_disnbs <- function(ibm,
                       run_scen = c("baseline", "impact", "baseline-and-impact"),
                       dens_id,
                       intake_id = NULL,
                       imp_dens_id = NULL,
                       imp_intake_id = NULL,
                       feed_state_id,
                       roost_state_id,
                       feed_avg_net_energy = units::set_units(422, "kJ/h"),
                       target_energy = units::set_units(1, "kJ"),
                       waypnts_res = 100, # units::set_units(100, "m")
                       smooth_body_mass = bm_smooth_opts(),
                       seed = sample(3000, 1),
                       quiet = FALSE){

  # TODO's
  # - implement a "disNBS" model_config type object to allow wrapping `rmr_run()`
  #   over this function
  # - Find a better solution for handling which drivers to use under baseline and
  #   impact scenarios

  if(isFALSE(quiet)) cli::cli_h1("Running the DisNBS Individual-Based Model")

  # input validation -------------------------
  if(isFALSE(quiet)) cli::cli_progress_step("Performing validation checks on inputs and underlying data.")

  ## Assert classes
  check_class(ibm, "IBM")
  check_class(dens_id, "character")
  check_class(feed_state_id, "character")
  check_class(roost_state_id, "character")
  if(not_null(imp_dens_id)) check_class(imp_dens_id, "character")
  if(not_null(imp_intake_id)) check_class(imp_intake_id, "character")

  if(inherits(waypnts_res, "numeric")) waypnts_res <- units::set_units(waypnts_res, "m")
  check_class(waypnts_res, "units")

  run_scen <- rlang::arg_match(run_scen)


  ## baseline Vs impacted scenarios: check required driver IDs are provided
  if(run_scen %in% c("baseline", "baseline-and-impact")){
    if(is.null(intake_id)){
      cli::cli_abort("{.arg intake_id} must be provided when {.code run_scen = {.val {run_scen}}}")
    }
  }

  if(run_scen %in% c("impact", "baseline-and-impact")){
    if(is.null(imp_dens_id)){
      cli::cli_abort("{.arg imp_dens_id} must be provided when {.code run_scen = {.val {run_scen}}}")
    }
    if(is.null(imp_intake_id)){
      cli::cli_abort("{.arg imp_intake_id} must be provided when {.code run_scen = {.val {run_scen}}}")
    }
  }


  ## check if drivers present in `ibm`
  drv_ids <- sapply(ibm@drivers, \(d) d@id)
  input_drv_ids <- switch(
    run_scen,
    baseline = c(dens_id, intake_id),
    impact = c(dens_id, imp_dens_id, imp_intake_id),
    'baseline-and-impact' = c(dens_id, intake_id, imp_dens_id, imp_intake_id),
  )

  nonexistent_drvs <- setdiff(input_drv_ids, drv_ids)

  if(length(nonexistent_drvs) > 0){
   cli::cli_abort(c(
     "Failed to find driver ID{?s} {.val {nonexistent_drvs}} in the provided {.cls IBM} object.",
     i = "Ensure all required driver IDs are present in slot {.field @drivers} of the {.arg ibm} object."
   ))
  }

  ## Drivers' @stars_obj must be non-empty and with only one attribute
  purrr::walk(
    input_drv_ids,
    function(id, call = rlang::caller_env()){
      drv_idx <- match(id, drv_ids)
      drv_strs <- ibm@drivers[[drv_idx]] |> stars_obj()

      if(is_stars_empty(drv_strs)){

        cli::cli_abort(
          "Slot {.code @stars_obj} of Driver {.val {id}} must contain a populated {.cls stars} object.",
          call = call
        )

      } else if(length(drv_strs) > 1){
        cli::cli_abort(c(
          "Slot {.code @stars_obj} of Driver {.val {id}} must have one unique attribute.",
          x = "Provided object has {length(drv_strs)} attributes: {.val {names(drv_strs)}}."
        ), call = call)
      }
    }
  )

  ## CRS consistency between model_config used datacubes.
  ## NOTE: this check is already performed in `rmr_initiate()`, but doing it
  ## again here for extra safety. Might drop this in the future
  purrr::walk(
    input_drv_ids,
    function(id, call = rlang::caller_env()){
      drv_strs <- pluck_s4(ibm@drivers, id) |> stars_obj()
      drv_crs <- sf::st_crs(drv_strs)

      if(drv_crs$proj4string != ref_sys(ibm)$proj4string){
        cli::cli_abort(c(
          "Driver {.val {id}} must have the same CRS as specified in slot {.code @model_config} of argument {.arg ibm}.",
          x = "Detected CRS of {.cls stars} object for {.val {id}}: {.val {drv_crs$Name}} (EPSG: {.val {drv_crs$epsg}})",
          x = "Expected CRS from {.code ibm@model_config@ref_sys}: {.val {ref_sys(ibm)$Name}} (EPSG: {.val {ref_sys(ibm)$epsg}})"
        ),
        call = call, class = "err-crs-mismatch")
      }
    }
  )


  ## baseline Vs impacted scenarios: check dimension consistency in density maps
  if(run_scen %in% c("impact", "baseline-and-impact")){

    # Check consistency of stars objects' dimensions. Required for extracting
    # slices of densities maps during simulations. Currently the construction of
    # slices is based on baseline density map, and assumes impact density maps
    # have identical non-raster dims and format
    dns_dim <- pluck_s4(ibm@drivers, dens_id) |> stars_obj() |> dim()
    imp_dns_dim <- pluck_s4(ibm@drivers, imp_dens_id) |> stars_obj() |> dim()

    if(any(dns_dim != imp_dns_dim)){
      idx <- which(dns_dim != imp_dns_dim)
      txt_l <- paste(paste(names(dns_dim), dns_dim, sep = ':'), collapse = ", ")
      txt_r <- paste(paste(names(imp_dns_dim), imp_dns_dim, sep = ':'), collapse = ", ")

      cli::cli_abort(c(
        "{.cls stars} objects of drivers {.val {c(dens_id, imp_dens_id)}} must have identical dimensions.",
        x = "Driver {.val {dens_id}} cube dimensions: [{txt_l}]",
        x = "Driver {.val {imp_dens_id}} cube dimensions: [{txt_r}]",
        i = "Note: make sure to ru {.fn rmr_initiate} after adjustments made to data contained in drivers."
      ))
    }
  }


  ## check if expected units are satisfied
  check_units_contextual(waypnts_res, context = "length")
  check_units_contextual(feed_avg_net_energy, context = "energy-time")
  check_units_contextual(target_energy, context = "energy")

  if(not_null(intake_id)){
    drv_vals <- stars_obj(pluck_s4(ibm@drivers, intake_id))[[1]]
    check_units_contextual(drv_vals, context = "energy-time", arg = "intake_id")
  }

  if(not_null(imp_intake_id)){
    drv_vals <- stars_obj(pluck_s4(ibm@drivers, imp_intake_id))[[1]]
    check_units_contextual(drv_vals, context = "energy-time", arg = "imp_intake_id")
  }


  # body mass smoother
  if(not_null(smooth_body_mass)){
    if(!inherits(smooth_body_mass, "bm_smooth_opts")){
      cli::cli_abort(c(
        "{.arg smooth_body_mass} must be an object f class {.cls bm_smooth_opts}.",
        i = "Create required object via {.fun bm_smooth_opts}."
      ))
    }
  }


  # Constraint on minimum model time step for "1 day". Otherwise, computations
  # dependent on daily night fraction, such as state_balance(), fall apart. As
  # it stands, this is always observed, as <ModelConfig>@delta_time forces any
  # input to be >= "1 day". This is therefore a safeguard for future devs where
  # delta_time is allowed to take shorter time lengths
  if(lubridate::period(ibm@model_config@delta_time) < lubridate::days(1)){
    cli::cli_abort(c(
      "The DisNBS model cannot run on time-steps smaller than 1 day.",
      i = "Adjust the slot `delta_time` of the {.cls ModelConfig} object and re-initialize the model via {.fn rmr_initiate}"
    ))
  }


  # feed_state_id and roost_state_id
  states_ids <- lapply(ibm@species@states_profile, \(s) s@id)
  nonidentified_states <- setdiff(
    list(feed_state_id = feed_state_id, roost_state_id = roost_state_id),
    states_ids
  )

  if(length(nonidentified_states) > 0){
    cli::cli_abort(c(
      "State{?s} ID{?s} {.val {unlist(nonidentified_states)}} {?is/are} not defined in the {.cls IBM} object provided to {.arg ibm}.",
     i = "Please ensure the input{?s} to {.arg {names(nonidentified_states)}} is listed in {.code ibm@species@states_profile}.",
     i = "Available state ID{?s} {?is/are} {.val {unlist(states_ids)}}."
    ))
  }


  # Prepare and further check data for simulation  ----------------------------------------

  if(isFALSE(quiet)) cli::cli_progress_step("Preparing and configuring data for simulation.")

  # get driver's indices in <IBM>, ordered as dens, intake, impacted dens, impacted intake
  drv_idx <- match(input_drv_ids, drv_ids)

  # check if density extent covers all agents' initial locations
  # TODO: this needs refinement, as being inside AOC doesn't guarantee a non-NA
  # density cell at the agent's starting position, as density maps don't cover
  # the whole extent of the raster
  agents_init_locs <- lapply(ibm@agents, location) |>
    sf::st_sfc(crs = ref_sys(ibm))

  n_agents_outside <- sf::st_covers(
    agents_init_locs,
    sf::st_as_sfc(sf::st_bbox(ibm@drivers[[drv_idx[1]]] |> stars_obj()))
  ) |>
    lengths() |>
    sum()

  if(n_agents_outside > 0){
    cli::cli_abort(c(
      "Density driver {.val {dens_id}} does not cover all agent starting locations.",
      x = "Found {cli::qty(n_agents_outside)} {n_agents_outside} agent{?s} starting outside the spatial extent of {.val {dens_id}}.",
      i = "Adjust {.arg @start_sites} in {.cls ModelConfig}, or provide a broader density surface.",
      i = "Then rerun {.fun rmr_initiate} to update the {.cls IBM} object used in {.arg ibm}."
    ))
  }


  # create config object for the agent simulation function
  cfg <- create_dnbs_config(
    dens_drv = pluck_s4(ibm@drivers, dens_id),
    ibm_cfg = ibm@model_config,
    waypnts_res = waypnts_res,
    ids = list( # congregate IDs, for tidyness
      dens_id = dens_id,
      intake_id = intake_id,
      imp_dens_id = imp_dens_id,
      imp_intake_id = imp_intake_id,
      feed_state_id = feed_state_id,
      roost_state_id = roost_state_id
    ),
    bmsm_opts = smooth_body_mass
  )

  # generate datacube with night-time proportions
  night_prop <- derive_night_cube(
    aoc_strs = pluck_s4(ibm@drivers, "aoc") |> stars_obj(),
    start_date = ibm@model_config@start_date,
    end_date = ibm@model_config@end_date,
    delta_time = "1 week"
  )



  #cli::cli_progress_update()
  # Simulate agents, individually over time  -----------------------------

  agents_bsln <- agents_imp <- NULL

  ## Baseline run -----------------------------------------
  if(run_scen %in% c("baseline", "baseline-and-impact")){

    if(isFALSE(quiet)) cli::cli_progress_step("Simulating agents' journeys under the baseline-case scenario")

    fmt <- "{cli::symbol$info} Simulating baseline scenario {cli::pb_bar} {cli::pb_current}/{cli::pb_total} Agents | Elapsed: {cli::pb_elapsed} | ETA: {cli::pb_eta}"

    set.seed(seed)
    agents_bsln <- furrr::future_map(
      #agents_bsln <- purrr::map(
      cli::cli_progress_along(1:length(ibm@agents), current = FALSE, format = fmt),
      sim_agent_wrapper,
      agents = ibm@agents,
      drivers = ibm@drivers,
      states_profile = ibm@species@states_profile,
      scen = "baseline",
      night_proportion = night_prop,
      dnbs_cfg = cfg,
      feed_avg_net_energy = feed_avg_net_energy,
      target_energy = target_energy,
      .options = furrr::furrr_options(
        seed = TRUE,
        packages = c("stars", "sf", "purrr", "units", "rlang", "cli", "dplyr", "terra", "spaths", "stats")
      )
    )
  }


  ## Impact run -------------------------------------------
  if(run_scen %in% c("impact", "baseline-and-impact")){

    if(isFALSE(quiet)) cli::cli_progress_step("Simulating agents' journeys under the impact-case scenario")

    fmt <- "{cli::symbol$info} Simulating impact scenario {cli::pb_bar} {cli::pb_current}/{cli::pb_total} Agents | Elapsed: {cli::pb_elapsed} | ETA: {cli::pb_eta}"

    set.seed(seed)
    agents_imp <- furrr::future_map(
      #agents_imp <- purrr::map(
      cli::cli_progress_along(1:length(ibm@agents), current = FALSE, format = fmt),
      sim_agent_wrapper,
      agents = ibm@agents,
      drivers = ibm@drivers,
      states_profile = ibm@species@states_profile,
      scen = "impact",
      night_proportion = night_prop,
      dnbs_cfg = cfg,
      feed_avg_net_energy = feed_avg_net_energy,
      target_energy = target_energy,
      .options = furrr::furrr_options(
        seed = TRUE,
        packages = c("stars", "sf", "purrr", "units", "rlang", "cli", "dplyr", "terra", "spaths", "stats")
      )
    )
  }


  ## Wrap up ---------------------------
  if(isFALSE(quiet)){
    cli::cli_progress_done()
    em <- sample(c('chequered', 'flight_arrival', 'tada'), 1)
    cli::cli_alert_success("Model simulation finished! {emoji::emoji(em)}")
    #cli::cli_text(cli::style_bold("{cli::symbol$star} Finished the simulation!"))
  }

  list(agents_bsln = agents_bsln, agents_imp = agents_imp)
}







# wrapper for agent simulating function handle automatic globals detection. In
# this specific case, failing to do this would export the whole `ibm` object to
# each of the workers, slowing down performace and increasing memory requirements
sim_agent_wrapper <- function(i, agents, drivers, states_profile, scen,
                              night_proportion, dnbs_cfg, feed_avg_net_energy,
                              target_energy){
  simulate_agent_disnbs(
    agent = agents[[i]],
    drivers = drivers,
    states_profile = states_profile,
    scen = scen,
    night_proportion = night_proportion,
    dnbs_cfg = dnbs_cfg,
    feed_avg_net_energy = feed_avg_net_energy,
    target_energy = target_energy
  )
}



#' Derive rasters of night-time proportion per day
#'
#' Helper to compute a <stars> datacube comprising spatio-temporal maps of
#' night-time proportions, within simulated AOC and over the simulated time
#' period
#'
#' NOTES:
#' - uses `geosphere::daylength()` which requires latitude values. Thus,
#' input `aoc_strs` object needs to be projected/resampled into "EPSG:4326" for
#' computations, before being projected back to its original CRS
#' - Temporal dimension of returned <stars> is always Date
#'
derive_night_cube <- function(aoc_strs, start_date, end_date, delta_time = "1 week"){

  # fetch "official" CRS
  crs <- sf::st_crs(aoc_strs)

  # re-project if CRS is not lat/lon
  reproject <- crs != sf::st_crs(4326)

  if(reproject){
    aoc_strs <- stars::st_warp(aoc_strs, crs = sf::st_crs(4326))
  }

  # value grids for required variables
  lon <- stars::st_get_dimension_values(aoc_strs, "x", where = "start")
  lat <- stars::st_get_dimension_values(aoc_strs, "y", where = "start")
  tgrd <- seq(start_date, end_date + lubridate::period(delta_time), by = delta_time)

  # derive night-time proportions for each latitude across time grid
  npr <- sapply(lat, \(x) 1 - geosphere::daylength(x, tgrd)/24)

  # generate 3D array of night-time props for coecinf
  npr <- array(rep(npr, length(lon)), dim = c(length(tgrd), length(lat), length(lon))) |>
     aperm(c(3, 2, 1))

  # coerce 3D array as <stars>
  npr_cube <- stars::st_as_stars(npr) |>
    stars::st_set_dimensions(1, names = "x", values = lon) |>
    stars::st_set_dimensions(2, names = "y", values = lat) |>
    stars::st_set_dimensions(3, names = "date", values = tgrd) |>
    setNames("night_fraction") |>
    sf::st_set_crs(4326)

  # re-project back to "official" CRS
  if(reproject){
    npr_cube <- stars::st_warp(npr_cube, crs = crs)
  }
  #plot(npr_cube, col = viridisLite::mako(50, direction = -1))

  npr_cube
}



#' Helper to extract data and info required for configuring DisNBS's agent
#' simulation model
#'
#' Quick notes, some of them worthy of a reference in details of parent function
#' `run_disnbs()`:
#'   - If density driver has no temporal dimension, then the density raster is the
#'   same across all model time-steps
#'   - currently looks for exact correspondence between time-grid and the values of
#'   temporal dimension of the density driver. This means any miss-match between
#'   the models expected time-steps and the available data will result in an
#'   error. Might make sense to relax this approach in future dev iterations,
#'   e.g. to take the nearest preceding density slice
#'
#' @returns an object of class <disnbs_config> with listed parameters required
#'   for input argument `config` of `simulate_agent_disnbs()`
#'
create_dnbs_config <- function(dens_drv,
                               ibm_cfg,
                               ids,
                               waypnts_res = units::set_units(100, "m"),
                               bmsm_opts = bm_smooth_opts(),
                               call = rlang::caller_env()){

  # COMEBAK: will need revisiting if/when implementing
  # asynchronous agent starting dates. Ideally via further processing inside
  # agents' simulation function
  time_grid <- seq(ibm_cfg@start_date, ibm_cfg@end_date, by = ibm_cfg@delta_time)

  # extract non-raster metadata density datacube from its parent driver's object
  dns_nrst_meta <- dens_drv@stars_meta$non_raster

  # initialize elements of interest
  dns_tm_slices <- NULL
  dns_tm_dim <- NA_integer_ # NAs for dimensions needed for slice_stars() to work
  dns_itr_dim <- NA_integer_
  dns_itr_slices <- NULL
  routing_timesteps <- 1

  if(not_null(dns_nrst_meta)){

    nrst_tm_idx <- which(dns_nrst_meta$types == "temporal")
    nrst_itr_idx <- which(dns_nrst_meta$types == "iteration")

    # process temporal dimension
    if(length(nrst_tm_idx) > 0){

      dns_tm_dim <- dns_nrst_meta$dims[nrst_tm_idx]
      tm_proc <- dns_nrst_meta$procs[nrst_tm_idx]
      tm_cl <- dns_nrst_meta$cls[nrst_tm_idx]
      tm_vals <- stars::st_get_dimension_values(dens_drv@stars_obj, dns_tm_dim)

      # map model time-grid to temporal dimension of density cube
      time_mapping <- if(tm_cl %in% c("Date", "POSIXct", "POSIXlt")){
        match(time_grid, tm_vals)
      } else {
        if(tm_proc == "month_chr"){
          pmatch(lubridate::month(time_grid, label = TRUE), tm_vals, duplicates.ok = TRUE)
        } else {
          # get components of time grid to match those expected under the temporal
          # dimension of the density surface
          tgrd <- switch(
            tm_proc,
            month_num = lubridate::month(time_grid),
            year = lubridate::year(time_grid),
            quarter = lubridate::quarter(time_grid),
            week = lubridate::week(time_grid),
            yday = lubridate::yday(time_grid),
          )
          match(tgrd, tm_vals)
        }
      }

      if(any(is.na(time_mapping))){
        cli::cli_abort(c(
          "Temporal dimension in datacube of driver {.val {dens_drv@id}} does not fully cover all time steps under modeling.",
          i = "Ensure the driver provides data for the entirety of the simulation period."
        ),
        call = call)
      }

      routing_timesteps <- c(1, which(diff(na.exclude(time_mapping)) != 0) + 1)
      dns_tm_slices <- unique(na.exclude(time_mapping))
    }

    # process iteration dimension
    if(length(nrst_itr_idx) > 0){
      dns_itr_dim <- dns_nrst_meta$dims[nrst_itr_idx]
      dns_itr_slices <- sample(dim(dens_drv@stars_obj)[dns_itr_dim], length(routing_timesteps), replace = TRUE)
    }
  }


  # bodymass smoother
  bm_smooth <- if(is.null(bmsm_opts)){
    list(
      apply = FALSE,
      ks_bw = NA_real_
    )
  }else{
    # translate bandwidth from literal time to model's time-steps
    steps_bw <- lubridate::period(bmsm_opts$time_bw)/lubridate::period(ibm_cfg@delta_time)
    list(
      apply = TRUE,
      ks_bw = steps_bw * 2
    )
  }

  # return object of class <disnbs_config>
  structure(
    append(
      list(
        time_grid = time_grid, # simulation's timepoints
        routing_timesteps = routing_timesteps, # timesteps (i.e. indices of simulation's timepoints) at which routing paths must be established
        dns_tm_dim = dns_tm_dim, # density datacube: temporal dimension
        dns_tm_slices = dns_tm_slices, # density datacube: slices of temporal dimension mapping each routing timestep
        dns_itr_dim = dns_itr_dim, # density datacube: iterative dimension
        dns_itr_slices = dns_itr_slices,
        step_drtn = units::as_units(ibm_cfg@delta_time),
        crs = ibm_cfg@ref_sys,
        aoc_bbx = ibm_cfg@aoc_bbx,
        waypnts_res = waypnts_res,
        bm_smooth = bm_smooth
      ),
      ids
    ),
    class = "disnbs_config"
  )

}



#' Set options for body mass smoother
#'
#'
#' @export
bm_smooth_opts <- function(time_bw = NULL, smoother = "NW-kernel"){

  if(is.null(time_bw)) return(NULL)

  smoother <- rlang::arg_match(smoother)

  structure(
    list(
      time_bw = time_bw,
      smoother = smoother
    ),
    class = "bm_smooth_opts"
  )

}


#  set.seed(seed)
#  agents_bsln <- furrr::future_map(
#  #agents_bsln <- purrr::map(
#    cli::cli_progress_along(ibm@agents, current = FALSE, format = fmt),
#    function(i){
#      #browser()
#      simulate_agent_disnbs(
#        agent = ibm@agents[[i]],
#        drivers = ibm@drivers,
#        states_profile = ibm@species@states_profile,
#        scen = "baseline",
#        night_proportion = night_prop,
#        dnbs_cfg = cfg,
#        feed_avg_net_energy,
#        target_energy
#      )
#    },
#    .options = furrr::furrr_options(
#      seed = TRUE,
#      packages = c("stars", "sf", "purrr", "units", "rlang", "cli", "dplyr", "terra", "spaths", "stats")
#      #chunk_size = 5
#    )
#  )
# }


# set.seed(seed)
# agents_imp <- furrr::future_map(
# #agents_imp <- purrr::map(
#   cli::cli_progress_along(ibm@agents, current = FALSE, format = fmt),
#   function(i){
#     simulate_agent_disnbs(
#       agent = ibm@agents[[i]],
#       drivers = ibm@drivers,
#       states_profile = ibm@species@states_profile,
#       scen = "impact",
#       night_proportion = night_prop,
#       dnbs_cfg = cfg,
#       feed_avg_net_energy,
#       target_energy
#     )
#   },
#   .options = furrr::furrr_options(
#     seed = TRUE,
#     packages = c("stars", "sf", "purrr", "units", "rlang", "cli", "dplyr", "terra", "spaths", "stats")
#     #chunk_size = 5
#   )
# )


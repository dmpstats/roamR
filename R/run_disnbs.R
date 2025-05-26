#' Run the DisNBS model
#'
#'
#' @param waypnts_res the distance between waypoints defining the movement path
#'   of simulated agents. Either a `<numeric>` value, expressed in meters, or an
#'   `<units>` object with a valid length units.
#'
#' @export
run_disnbs <- function(ibm,
                       dens_id,
                       intake_id,
                       feed_state_id,
                       roost_state_id,
                       run_scen = c("baseline", "impact", "baseline-and-impact"),
                       imp_dens_id = NULL,
                       imp_intake_id = NULL,
                       waypnts_res = 100 # units::set_units(100, "m")
                       ){

  # TODO's
  # - Add constraint on minimum model time step for "1 day". Otherwise,
  #   computations dependent on daily night fraction, such as state_balance(),
  #   fall apart.
  # - implement a "disNBS" model_config type object to allow wrapping `rmr_run()`
  #   over this function
  # - Find a better solution for handling which drivers to use under baseline and
  #   impact scenarios

  # Topline input validation -------------------------

  ## Assert classes
  check_class(ibm, "IBM")
  check_class(dens_id, "character")
  if(!is.null(imp_dens_id)) check_class(imp_dens_id, "character")
  if(!is.null(imp_intake_id)) check_class(imp_intake_id, "character")

  if(inherits(waypnts_res, "numeric")) waypnts_res <- units::set_units(waypnts_res, "m")
  check_class(waypnts_res, "units")


  run_scen <- rlang::arg_match(run_scen)

  if(!units::ud_are_convertible(units(waypnts_res), "m")){

    cli::cli_abort(c(
      "{.arg waypnts_res} must be specified with a valid unit of length.",
      x = "{.val {units::deparse_unit(waypnts_res)}} is not recognized as a length unit.",
      i = "Use e.g., {.val m} or {.val km} instead."
    ))
  }


  ## check if drivers present in `ibm`
  drv_ids <- sapply(ibm@drivers, \(x) x@id)
  input_drv_ids <- c(dens_id, intake_id, imp_dens_id, imp_intake_id)

  nonexistent_drvs <- setdiff(input_drv_ids, drv_ids)

  if(length(nonexistent_drvs) > 0){
   cli::cli_abort(c(
     "Driver ID{?s} {.val {nonexistent_drvs}} not found in the provided {.cls IBM} object.",
     i = "Ensure all specified driver IDs are present in slot {.field @drivers} of the {.arg ibm} object."
   ))
  }


  # Drivers' @stars_obj must be non-empty and with only one attribute
  purrr::walk(
    input_drv_ids,
    function(id){
      drv_idx <- match(id, drv_ids)
      drv_strs <- ibm@drivers[[drv_idx]] |> stars_obj()

      if(is_stars_empty(drv_strs)){

        cli::cli_abort("Slot @stars_obj of Driver {.val {id}} must contain a populated {.cls stars} object.")

      } else if(length(drv_strs) > 1){

        cli::cli_abort(c(
          "Slot @stars_obj of Driver {.val {id}} must have one unique attribute.",
          x = "Provided object has {length(drv_strs)} attributes: {.val {names(drv_strs)}}."
        ))
      }
    }
  )


  # Prepare and further check data for simulation  ----------------------------------------

  # TODO:
  #  - check if dens and imp_dens have consistent dimensions and attribute units
  #  - CRS consistency between "official" and density and intake datacubes.
  #   Transform if different?
  #  - impose check on expected units and convertibility (e.g. energy, time-budget in proportion)


  # get driver's indices in <IBM>, ordered as dens, intake, impacted dens, impacted intake
  drv_idx <- match(input_drv_ids, drv_ids)

  # check if density extent covers all agents' initial locations
  # TODO: this needs improving as being inside AOC doesn't guarantee a non-NA
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
      feed_state_id = "foraging",
      roost_state_id = "water_resting"
    )
  )


  # generate datacube with night-time proportions
  night_prop <- derive_night_cube(
    aoc_strs = pluck_s4(ibm@drivers, "aoc"),
    start_date = ibm@model_config@start_date,
    end_date = ibm@model_config@end_date,
    delta_time = "1 week"
  )



  # Simulate agents, individually over time  -----------------------------
  agents_bsln <- agents_imp <- NULL

  ## baseline run ------------------------

  agents_bsln <- if(run_scen %in% c("baseline", "baseline-and-impact")){

    # run agent-level simulation
    #out_bsln <- furrr::future_map(
     purrr::map(
      ibm@agents, # [1:10],
      function(a){
        simulate_agent_disnbs(
          agent = a,
          drivers,
          drv_ids,
          states_profile,
          scen = "baseline",
          night_prop,
          dnbs_cfg
        )
      },
      .progress = TRUE
      #.options = furrr::furrr_options(seed = TRUE)
    )
  }

  ## Impact run ------------------------
  agents_imp <- if(run_scen %in% c("impact", "baseline-and-impact")){
    # run agent-level simulation
    #out_bsln <- furrr::future_map(
    purrr::map(
      ibm@agents, # [1:10],
      function(a){
        simulate_agent_disnbs(
          agent = a,
          drivers,
          drv_ids,
          states_profile,
          scen = "impact",
          night_prop,
          dnbs_cfg
        )
      },
      .progress = TRUE
      #.options = furrr::furrr_options(seed = TRUE)
    )
  } else{
    NULL
  }



  list(agents_bsln = agents_bsln, agents_imp = agents_imp)

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
  lon <- stars::st_get_dimension_values(aoc_strs, "x")
  lat <- stars::st_get_dimension_values(aoc_strs, "y")
  tgrd <- seq(start_date, end_date, by = delta_time)

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



#' Helper to extract data and info required for configuring the DisNBS model
#'
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
                               call = rlang::caller_env()){

  # TODO:
  # - check on if required IDs are provided on `ids`

  # COMEBAK: will need revisiting if/when implementing
  # asynchronous agent starting dates. Ideally via further processing inside
  # agents' simulation function
  time_grid <- seq(ibm_cfg@start_date, ibm_cfg@end_date, by = ibm_cfg@time_step)

  # extract non-raster metadata density datacube from its parent driver's object
  dns_nrst_meta <- dens_drv@stars_meta$non_raster

  # initialize elements of interest
  dns_tm_slices <- NULL
  dns_tm_dim <- NA_integer_ # NAs for dimensions needed for slice_stars() to work
  dns_itr_dim <- NA_integer_
  dns_itr_slices <- NULL
  routing_timesteps <- 1

  if(!is.null(dns_nrst_meta)){

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
      dns_itr_slices <- 1:dim(dens_drv@stars_obj)[dns_itr_dim]
    }
  }


  #names(ids) %in% c()

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
        step_drtn = units::as_units(ibm_cfg@time_step),
        crs = ibm_cfg@ref_sys,
        waypnts_res = waypnts_res
      ),
      ids
    ),
    class = "disnbs_config"
  )

}


#' Run Agent-level simulation under the DisNBS model
#'
#' Executes an individual-based simulation for a single `<Agent>` object under
#' the DisNBS  model framework. This function models the agent's spatial
#' movement and physiological condition over the simulation period. Movement is
#' based on density maps using the shortest path method. Energy intake maps are
#' used for balancing out energy expenditure.
#'
#' Notes to expand:
#'  - Assumes movement is exclusively one-directional, towards the end of each track
#'  - once agent gets to a track's endpoint, it stays there until the following
#'  rerouting step, relardless of of the travelling distance returned from the
#'  states
#'
#' @param agent `<Agent>`, representing the individual to be simulated.
#' @param dens `<stars>`, providing the spatio-temporal species density
#'   distribution used to drive the agent's movement.
#' @param intake_drv `<stars>`, containing the energy intake surface for the
#'   agent. Used to simulate energetic dynamics.
#' @param dnbs_cfg TODO
#'
#' @returns A modified `<Agent>` object containing the complete simulated
#'   trajectory and condition history of the agent over the simulation period,
#'   based on the provided inputs.
simulate_agent_disnbs <- function(agent,
                                  drivers,
                                  states_profile,
                                  scen = c("baseline", "impact"),
                                  night_proportion,
                                  dnbs_cfg,
                                  feed_avg_net_energy,
                                  target_energy = units::set_units(1, "kJ")){

  # TODO
  # - Improve calculation of step travelled distance based on a correlated random
  #   walk
  # - nudging final positions once end of track is reached
  # - further factorisation?

  # check ------------------------------------------------------
  if (!inherits(dnbs_cfg, "disnbs_config")){
    cli::cli_abort(c(
      "{.arg dnbs_cfg} must be an object of class {.cls disnbs_config}.",
      i = "Build required object via {.fun create_dnbs_config}."
    ))
  }

  scen <- rlang::arg_match(scen)

  # prep ------------------------------------------------------

  # rename list elements to actually defined state IDs
  names(states_profile) <- sapply(states_profile, \(s) s@id)

  # set up agent's impacted status and ID of intake driver
  if(scen == "baseline"){
    impacted <- FALSE
    intake_id <- dnbs_cfg$intake_id
  }else{
    stopifnot(not_null(dnbs_cfg$imp_dens_id))
    stopifnot(not_null(dnbs_cfg$imp_intake_id))

    impacted <- agent@properties@move_influences[[dnbs_cfg$imp_dens_id]]$infl
    intake_id <- dnbs_cfg$imp_intake_id
  }

  # Units management
  step_drtn_hrs <- dnbs_cfg$step_drtn |>
    assert_units_to_numeric("hr")

  states_budget(agent@condition) <- lapply(
    states_budget(agent@condition),
    \(s){ units(s) <- NULL; s}
  )

  feed_avg_net_energy <- assert_units_to_numeric(feed_avg_net_energy, "kJ/h")
  target_energy <- assert_units_to_numeric(target_energy, "kJ")
  energy_to_mass <- assert_units_to_numeric(agent@properties@energy_to_mass, "g/kJ")

  # convert speeds to meters/hr for calculations below
  # NOTE: speeds currently fixed for each agent -
  agent_speeds <- purrr::modify_if(
    agent@properties@speeds,
    Negate(is.na),
    \(x) assert_units_to_numeric(x, "meters/hour")
  )

  # initiate ------------------------------------------------------

  # list object to store agent's data from each step, for later assignment to
  # Agent@history
  hist <- list()

  track_id <- 0L

  # initiate agent's history
  # NOTE: currently overwrites object from Agent's initialization step in
  # rmr_initiate()
  hist_0 <- sf::st_sf(
    timestep = 0L,
    track_id = track_id,
    body_mass = body_mass(agent),
    states_budget = agent@condition@states_budget,
    states_unit_cost = agent@condition@states_cost,
    energy_expenditure = agent@condition@energy_expenditure,
    #dist_travelled = units::set_units(0, "m"),
    #prop_track = units::set_units(0, ""),
    geometry = sf::st_sfc(location(agent))
  )

  # initial location as sf
  step_loc <- sf::st_sf(
    tm = dnbs_cfg$time_grid[1],
    geometry = sf::st_sfc(location(agent), crs = dnbs_cfg$crs)
  )

  # run -----------------------------------------------------

  # run for-loop over simulations timepoints
  for(step in seq_along(dnbs_cfg$time_grid)){ # t = 1

    ## Generate track ---------------------------------------------------------

    # re-routing done at "start of the day", i.e. before moving the agent on
    # current delta_time
    if(step %in% dnbs_cfg$routing_timesteps){

      #if(track_id == 3) browser()

      # generate new track
      actv_track <- if(!impacted){
        calculate_track(
          agent,
          dens = extract_dns_layer(
            pluck_s4(drivers, dnbs_cfg$dens_id) |> stars_obj(),
            dnbs_cfg,
            step
          ),
          impacted = FALSE,
          crs = dnbs_cfg$crs,
          aoc_bbx = dnbs_cfg$aoc_bbx
        )
      }else{
        calculate_track(
          agent,
          dens = extract_dns_layer(
            pluck_s4(drivers, dnbs_cfg$dens_id) |> stars_obj(),
            dnbs_cfg,
            step
          ),
          impacted = TRUE,
          imp_dens = extract_dns_layer(
              pluck_s4(drivers, dnbs_cfg$imp_dens_id) |> stars_obj(),
              dnbs_cfg,
              step
          ),
          crs = dnbs_cfg$crs,
          aoc_bbx = dnbs_cfg$aoc_bbx
        )
      }

      # segmentize track to specified point resolution
      actv_waypnts <- actv_track |>
        sf::st_segmentize(dfMaxLength = dnbs_cfg$waypnts_res) |>
        sf::st_cast("POINT")

      # cumulative length of new track's waypoints (meters)
      cum_dist <- sf::st_distance(actv_waypnts, actv_waypnts[1]) |>
        assert_units_to_numeric("m")

      # reset track's total distance travelled by the agent (meters)
      total_dist <- 0
      #plot(actv_track, axes = TRUE)
      track_id <- track_id + 1L
    }


    ## Derive energetics ---------------------------------------------------
    # (at start of current step, given condition at previous)

    # energy-intake per unit-time (kJ/h) given current location. Based on user-provided
    # energy map
    unit_gain <- get_driver_cell_value(pluck_s4(drivers, intake_id), agent) |>
      assert_units_to_numeric("kJ/h")

    #if(is.na(unit_gain)) browser()

    # Current energy intake (kJ), given state budgets from previous step
    #
    # HACK: this is a temporary solution to convert unit_gain (kJ/h) into actual
    # energy gain (Joules) within this function. Several undesirable
    # constraints, e.g. energy gains limited to one state, doesn't fit with the
    # framework structure, etc. Ideally this should be done via a
    # State@energy_gain, analogous to the already existing State@energy_cost. Or
    # maybe the concatenation of the two into a single State@net_energy?
    energy_gain <- prod(
      agent@condition@states_budget[[dnbs_cfg$feed_state_id]],
      step_drtn_hrs,
      unit_gain
    )

    # energy-costs per unit-time (kJ/h) given current location
    state_unit_costs <- estimate_costs(agent, states_profile, drivers) |>
      lapply(function(s) assert_units_to_numeric(s, "kJ/h") )


    # Current total energy cost (kJ), given state budgets from previous step
    #
    # expects two objects to list states in same order. They should, as they
    # were initiated based on same list object during `rmr_initiate()`
    state_costs <- purrr::map2(
      state_unit_costs,
      states_budget(agent@condition),
      \(unit_cost, budget) unit_cost * step_drtn_hrs * budget
    )

    # step net total energy (kJ)
    net_energy <- Reduce(`+`, state_costs) + energy_gain

    # change in body mass (gr)
    step_mass_delta <- net_energy * energy_to_mass

    ## Rebalance states   -----------------------------------------------------
    # at start of the step, based on current energetic demands

    # get night-time fraction at current step
    night_prop <- stars::st_extract(night_proportion, step_loc, time_column = "tm")[[1]]

    # # hack to deal with NAs in night_prop
    # if(is.na(night_prop)){
    #   #browser()
    #   step_loc <- nudge_pnt_into_bbox(step_loc, sf::st_bbox(night_proportion))
    #   night_prop <- stars::st_extract(night_proportion, step_loc, time_column = "tm")[[1]]
    # }

    if(is.na(net_energy)) browser()

    # rebalance states budgets based on energetics
    states_budget(agent@condition) <- rebalance_states(
      states_budget(agent@condition),
      night_prop,
      dnbs_cfg$feed_state_id,
      dnbs_cfg$roost_state_id,
      net_energy,
      feed_avg_net_energy,
      target_energy,
      step_drtn_hrs
    )


    ## Move agent  -----------------------------------------------------

    # calculate distance (meters) travelled on current track by the end of step,
    # based on states budgets an their speeds
    step_dist <-  purrr::map2(
      agent_speeds,
      states_budget(agent@condition),
      \(speed, budget) speed * budget * step_drtn_hrs) |>
      purrr::discard(is.na) |>
      purrr::reduce(sum)

    # track's total distance covered by the end of step (meters)
    total_dist <- total_dist + step_dist

    # agent's location at the end of the step
    step_loc <- actv_waypnts[which.min(abs(total_dist - cum_dist))] |>
      st_sf(tm = dnbs_cfg$time_grid[step], geometry = _)

    # step_travel <- runif(1, 0.1, 1) * step_dist
    # total_dist <- total_dist + step_travel
    # step_loc <- actv_waypnts[which.min(abs(total_dist - cum_dist))] |>
    #   st_sf(tm = dnbs_cfg$time_grid[step], geometry = _)


    ## Update Agent slots ------------------------------------------
    # (at the end of the step)

    location(agent) <- sf::st_geometry(step_loc)[[1]]
    agent@condition@timestep <- step
    agent@condition@timestamp <- as.POSIXct(dnbs_cfg$time_grid[step], "UTC")
    agent@condition@energy_expenditure <- units::set_units(net_energy, "kJ")
    agent@condition@states_cost <- lapply(state_unit_costs, units::set_units, "kJ/h")

    step_mass_delta <- units::set_units(step_mass_delta, "g")
    agent@condition@mass_change_value <- step_mass_delta
    body_mass(agent) <- agent@properties@initial_mass + step_mass_delta

    hist[[ step ]] <- sf::st_sf(
      timestep = agent@condition@timestep,
      track_id = track_id,
      body_mass = body_mass(agent),
      states_budget = agent@condition@states_budget,
      states_unit_cost = agent@condition@states_cost,
      energy_expenditure = agent@condition@energy_expenditure,
      geometry = sf::st_sfc(location(agent))
    )
    #plot(history(agent)["timestep"])
  }

  hist <- do.call(rbind, hist)

  if(isTRUE(dnbs_cfg$bm_smooth$apply)){
    hist <- hist |>
      dplyr::mutate(
        body_mass_smooth = smooth_body_mass(timestep, body_mass, dnbs_cfg$bm_smooth$ks_bw),
        .after = body_mass
      )
  }

  history(agent) <- dplyr::add_row(hist, hist_0, .before = 1) |>
    sf::st_set_crs(dnbs_cfg$crs)

  agent
}





#' wrapper on slice_strs() to slice density surface with appropriate indices.
#' Slice construction relies on `cfg` to define the available non-raster
#' dimensions to evaluate
#'
#' @param dns_strs <stars> object containing the density datacube
extract_dns_layer <- function(dns_strs, cfg, timestep){

  if(!inherits(cfg, "disnbs_config")){
    stop("`cfg` must be object created via {.fun create_dnbs_config}.")
  }

  # get the index of the track corresponding to the input timestep
  route_idx <- which(cfg$routing_timesteps == timestep)

  dns_nrst_dims <- c(cfg$dns_tm_dim, cfg$dns_itr_dim)

  dns_slcs <- list(
    tm = cfg$dns_tm_slices[route_idx],
    itr = if(!is.na(cfg$dns_itr_dim)) cfg$dns_itr_slices[route_idx] else NULL
  )

  slice_strs(dns_strs, dns_nrst_dims, !!!dns_slcs, .drop = TRUE)
}




#' Shortest path calc
#'
#' @param agent A roamR agent - mainly for its current location
#' @param dens A raster density map to sample from
#' @param crs TODO
#' @param impacted TODO
#' @param imp_dens TODO
#'
#' @returns A track
#'
#' @examples TBD

calculate_track <- function(agent, dens, impacted = FALSE, crs, aoc_bbx, imp_dens = NULL) {

  # current location of the agent
  start <- sf::st_sfc(agent@condition@location, crs = crs)

  # generate candidate endpoint based on baseline density surface
  end <- sample_cell(dens, 1) |>
    sf::st_point() |>
    sf::st_sfc(crs = crs)

  # adjust endpoint if it happens to be on the border or outside the AOC. This
  # may happen due to the AOC cropping applied in initiation leaving a buffer
  # outside the AOC border, at which endpoints can end up. This nudging ensures
  # all endpoint are inside the AOC,
  if(!pnt_inside_bbox(end, aoc_bbx)){
    #browser()
    end <- nudge_pnt_into_bbox(end, aoc_bbx)
  }

  # logic for impacted scenario
  #' #' NOTES:
  #' #' This approach attempts to avoid large deviations in tracks between baseline
  #' #' and impacted scenarios, by forcing endpoints to be closer
  if(impacted){

    if(is.null(imp_dens)) cli::cli_abort("imp_dens must be non-null if `impacted`== TRUE")

    # extract endpoint cell value in impacted surface
    # NOTE: experimented with `st_cell()` but, for unclear reasons, the returned
    # cell index appeared to be one pixel short, leading to unexpected outcomes.
    # `stars::st_extract()` is less efficient, but behaves as expected
    imp_end_cell_val <- stars::st_extract(imp_dens, end)[[1]]

    # if cell value is NA, relocate endpoint to the closest populated cell in the impacted density surface
    if(is.na(imp_end_cell_val)){
      imp_dens_sf <- sf::st_as_sf(imp_dens, as_points = TRUE)
      new_end_imp_dens_idx <- sf::st_nearest_feature(end, imp_dens_sf)
      end <- imp_dens_sf$geometry[new_end_imp_dens_idx]
      rm(imp_dens_sf) # garbage collection
    }
  }

  # set spatial raster for shortest_paths()
  dens_rst <- if(impacted){
    terra::rast(imp_dens)
  }else {
    terra::rast(dens)
  }

  # Note: experimentation showed that passing a matrix to `rst` was ~2x faster
  # than using a <terra> object. However, each approach appeared to make
  # different decisions regarding endpoint linkage, resulting in different
  # linestring outputs. The <terra> approach was chosen, as it is recommended in
  # the documentation for Earth-related applications.
  spaths::shortest_paths(
    dens_rst,
    origins = end,
    destinations = start,
    output = "lines"
  ) |>
    sf::st_as_sf() |>
    sf::st_geometry()
}





#' Sampling a density map as PDF
#'
#' @param x A `<stars>` object. The first attribute is used to convert to a
#'   probability surface, based on which sampling is performed.
#' @param n How many samples to draw
#'
#' @returns A matrix of x,y coordinates, which are the centre of the sampled raster cells
#'
#' @examples
#'
#' x <- data.frame(expand.grid(x=1:5, y = 1:5), z = rlnorm(25)) |>
#'      stars::st_as_stars()
#'
#' sample_cell(x, 10)
#'
sample_cell <- function(strs, n = 1){

  rast_vect <- as.vector(strs[[1]])

  if(any(rast_vect < 0, na.rm = TRUE)){
    cli::cli_abort("All values in the first attribute of {.arg x} must be non-negative.")
  }

  rast_vect[is.na(rast_vect)] <- 0

  p_vect <- rast_vect/sum(rast_vect)

  samp_ind <- sample(1:length(p_vect), size = n, prob = p_vect, replace = TRUE)

  coords <- sf::st_coordinates(strs)[samp_ind, ]

  # ensure outputs a matrix. NB: runs faster than `as.matrix(rownames.force = TRUE)`
  data.matrix(coords)
}





#' Estimate energy costs for each state in the profile, given agents condition
#' (e.g. location, bodymass, etc)

#' @returns A named list, one element for each state id
#'
#' @examples TBD
estimate_costs <- function(agent, states_profile, drivers) {

  lapply(states_profile, function(state){
    cost <- if(is(state@energy_cost, "VarFn")){
      state@energy_cost@fn_cmp(agent, drivers) |>
        units::set_units(state@energy_cost@units, mode = "standard")
    } else {
      agent@properties@cost_par_draws[[state@id]]
    }

    -(cost)
  })

}



#' Rebalance activity states
#'
#' @param states_budget A list of time budget allocated to each state (relative proportion)
#' @param night_prop Proportion of day that is night (roost state cannot fall below this)
#' @param feed_state_id ID of the feeding state, providing energy intake
#' @param roost_state_id ID of the roosting state (where night constraint applies)
#' @param curr_energy net energy level at the current time-step (kJ)
#' @param feed_avg_net_energy average net energy intake per unit of time feeding (e.g. kJ/hr)
#' @param target_energy cumulative net energy target (kJ)
#' @param step_duration duration of the simulation time-step (e.g 1 day)
#'
#' @returns A list of time budgets allocated to each state (as relative proportions)
#'
#' NOTES:
#'  - Only applicable to diurnal species
#'
#' @examples TBD
rebalance_states <- function(states_budget,
                             night_prop,
                             feed_state_id,
                             roost_state_id,
                             curr_energy,
                             feed_avg_net_energy,
                             target_energy,
                             step_duration){

  out_states <- states_budget

  # lower and upper bounds of feed budget, as proportion of step duration
  feed_upper <- 1 - night_prop
  feed_lower <- 0

  # net energy demand
  net_target_energy <- target_energy - curr_energy

  # feeding duration required to meet energy demand
  feed_hrs <- net_target_energy / feed_avg_net_energy


  # Stage 1 adjustment: meet intake demands -----

  # proportion of time-step required to be spent feeding to meet energy intake demand
  feed_prop <- feed_hrs/step_duration
  feed_prop <- min(feed_prop, feed_upper)
  feed_prop <- max(feed_prop, feed_lower)

  out_states[[feed_state_id]] <- feed_prop

  non_feed_ids <- setdiff(names(states_budget), feed_state_id)

  non_feed_mult <- (1 - out_states[[feed_state_id]]) / Reduce(`+`, states_budget[non_feed_ids])
  out_states[non_feed_ids] <- lapply(states_budget[non_feed_ids], \(x) x * non_feed_mult)


  # Stage 2 adjustment: add roosting restriction (i.e. daily night-length) -----

  # if(is.na(night_prop)) browser()
  # if(is.null(night_prop)) browser()
  # if(is.na(out_states[[roost_state_id]])) browser()
  # if(is.null(out_states[[roost_state_id]])) browser()

  if(out_states[[roost_state_id]] < night_prop){

    out_states[[roost_state_id]] <- night_prop

    drop_states <- c(feed_state_id, roost_state_id)
    keep_states <- setdiff(names(states_budget), drop_states)

    remainder_states_mult <- (1 - Reduce(`+`, out_states[drop_states])) / Reduce(`+`, states_budget[keep_states])

    out_states[keep_states] <- lapply(states_budget[keep_states], \(x) x * remainder_states_mult)
  }

  out_states

}




#' Title Energy to mass conversion
#'
#' @param x A vector of values (the time steps)
#' @param e_vect A vector of energy values
#' @param bw A bandwidth for smoothing
#'
#' @returns A vector of wts
#'
#' @examples TBD
#'
smooth_body_mass <- function(steps, bmass, bw){

  ksm <- stats::ksmooth(x = steps, y = bmass, kernel = "normal", bandwidth = bw)

  units::set_units(ksm$y, "g")
}





# is point literally inside the bounding box? Returns FALSE if it's on the box's
# border or outside it
pnt_inside_bbox <- function(pnt, bbx){

  x_geom <- sf::st_geometry(pnt)

  stopifnot(length(x_geom) == 1)
  stopifnot(sf::st_is(x_geom, "POINT"))
  stopifnot(inherits(bbx, "bbox"))

  pnt <- x_geom[[1]]

  as.logical(
    pnt[[1]] > bbx["xmin"] &
      pnt[[1]] < bbx["xmax"] &
      pnt[[2]] > bbx["ymin"] &
      pnt[[2]] < bbx["ymax"]
  )
}



nudge_pnt_into_bbox <- function(x, bbx, eps = 0.01){

  x_geom <- sf::st_geometry(x)

  stopifnot(length(x_geom) == 1)
  stopifnot(sf::st_is(x_geom, "POINT"))
  stopifnot(inherits(bbx, "bbox"))

  pnt <- x_geom[[1]]

  new_xy <- purrr::pmap_dbl(
    list(
      min = c("xmin", "ymin"),
      max = c("xmax", "ymax"),
      val = c(pnt[[1]], pnt[[2]])
    ),
    function(min, max, val){
      #browser()
      if( (val <= bbx[[min]]) | (val >= bbx[[max]]) ){
        lims <- bbx[c(min, max)]
        lim_nm <- names(lims[which.min(abs(lims - val))])
        nudge <- abs(bbx[[lim_nm]]) * eps
        if(lim_nm %in% c("xmin", "ymin")){
          #nudge <- abs(bbx[[lim_nm]]) * eps
          bbx[[lim_nm]] + nudge
        }else{
          #nudge <- abs(bbx[[lim_nm]]) * eps
          bbx[[lim_nm]] - nudge
        }
      }else{
        val
      }
    }
  )

  new_pnt <- sf::st_point(new_xy, dim = "XY")

  # overwrite point coords in original object
  if(inherits(x, "sf")){
    sf::st_geometry(x)[[1]] <- new_pnt
  }else{
    x[[1]] <- new_pnt
  }

  x
}






assert_units_to_numeric <- function(x, units){

  stopifnot(inherits(x, "units"))

  units::set_units(x, units, mode = "standard") |>
    units::drop_units()
}




# # option 2 for finding current location on path via line interpolation.
# Requirement: path should be in UTM, which implies all spatial objects to be
# transformed to UTM upfront (i.e. in run_disnbs()) for optimal performance
#
# if(sf::st_is_longlat(actv_wayline)){
#   actv_wayline <- sf::st_transform(actv_wayline, crs = 32629)
# }
#
# #sf::st_length(actv_wayline)
#
# # inherent units in meters
# step_dist <- 1000 # runif(1, 10, 50)
#
# total_dist <- total_dist + step_dist
#
# step_location <- sf::st_line_interpolate(actv_wayline, total_dist)
#
# if(total_dist > sf::st_length(actv_wayline)){
#   sf::st_jitter(step_location, 100)
# }

# if( (pnt[[1]] < bbx[["xmin"]]) | (pnt[[1]] > bbx[["xmax"]]) ){
#   nm <- names(which.min(bbx[c("xmin", "xmax")] - pnt[[1]]))
#   if(nm == "xmin"){
#     new_x <- bbx[[nm]] * (1 + eps)
#   }else{
#     new_x <- bbx[[nm]] * (1 - eps)
#   }
# }else{
#   new_x <- pnt[[1]]
# }
#
#
# if ( (pnt[[2]] < bbx[["ymin"]]) | (pnt[[2]] > bbx[["ymax"]]) ) {
#   nm <- names(which.min(bbx[c("ymin", "ymax")] - pnt[[2]]))
#   if(nm == "ymin"){
#     new_y <- bbx[[nm]] * (1 + eps)
#   }else{
#     new_y <- bbx[[nm]] * (1 - eps)
#   }
# } else {
#   new_y <- pnt[[2]]
# }

# bbx |> sf::st_as_sfc() |> sf::st_nearest_points(x_geom)



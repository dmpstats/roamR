# DEPRECATED FUNCTION WHERE UNITS WERE KEPT IN CALCULATIONS, WHICH HAD AN IMPACT
# ON PERFORMNCE. REPLACING FUNCTION SETS UP UNITS UPFRONT BEFORE DROPPING THEM
# FOR USAGE IN CALCULATIONS. THIS DEPRECATED VERSION IS ~25% SLOWER THAN
# REPLACEMENT VERSION

#' Run Agent-level simulation under the DisNBS model
#'
#' Executes an individual-based simulation for a single `<Agent>` object under
#' the DisNBS  model framework. This function models the agent's spatial
#' movement and physiological condition over the simulation period. Movement is
#' based on density maps using the shortest path method. Energy intake maps are
#' used for balancing out energy expenditure.
#'
#' Notes to expand:
#'  - Assumes movement is exclusively one-directional, towards the end of each path
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
#'
#' @return A modified `<Agent>` object containing the agent's full simulated
#'   trajectory and condition history over the simulation period, based on the
#'   provided inputs.

simulate_agent_disnbs <- function(agent,
                                  drivers,
                                  states_profile,
                                  scen = c("baseline", "impact"),
                                  night_prop,
                                  dnbs_cfg,
                                  feed_avg_net_energy){

  # TODO
  # - implement body-mass reconstruction based on smoother
  # - improve efficiency by streamlining handling of units
  # - nudging final positions once end of path is reached
  # - further factorisation?

  # check ------------------------------------------------------
  if (!inherits(dnbs_cfg, "disnbs_config")){
    cli::cli_abort(c(
      "{.arg dnbs_cfg} must be an object of class {.cls disnbs_config}.",
      i = "Build required object via {.fun create_dnbs_config}."
    ))
  }

  # if (impacted && (is.null(imp_intake_drv) || is.null(imp_intake_drv))){
  #   cli::cli_abort(
  #     "{.arg impacted = TRUE} requires non-`NULL` values for {.arg imp_dens_drv} and {.arg imp_intake_drv}."
  #   )
  # }

  scen <- rlang::arg_match(scen)

  # prep ------------------------------------------------------

  # rename list elements to actually defined state IDs
  names(states_profile) <- sapply(states_profile, \(s) s@id)

  if(scen == "baseline"){
    impacted <- FALSE
    intake_id <- dnbs_cfg$intake_id
  }else{
    impacted <- agent@properties@move_influences[[dnbs_cfg$imp_dens_id]]$infl
    intake_id <- dnbs_cfg$imp_intake_id
  }


  # initiate ------------------------------------------------------

  # list object to store agent's data from each step, for later assignment to
  # Agent@history
  hist <- list()

  # initiate agent's history
  # NOTE: currently overwrites object from Agent's initialization step in
  # rmr_initiate()
  hist[[1]] <- sf::st_sf(
    timestep = 0L,
    body_mass = body_mass(agent),
    states_budget = agent@condition@states_budget,
    states_unit_cost = agent@condition@states_cost,
    energy_expenditure = agent@condition@energy_expenditure,
    #dist_travelled = units::set_units(0, "m"),
    #prop_path = units::set_units(0, ""),
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

    # re-routing done at "start of the day", i.e. before moving the agent on
    # current time_step
    if(step %in% dnbs_cfg$routing_timesteps){

      # generate new path
      actv_path <- if(!impacted){
        calculate_path(
          agent,
          dens = extract_dns_layer(
            pluck_s4(drivers, dnbs_cfg$dens_id) |> stars_obj(),
            dnbs_cfg,
            step
          ),
          impacted = FALSE,
          crs = dnbs_cfg$crs
        )
      }else{
        calculate_path(
          agent,
          dens = extract_dns_layer(
            pluck_s4(drivers, dnbs_cfg$dens_id) |> stars_obj(),
            dnbs_cfg,
            step
          ),
          impacted = TRUE,
          imp_dens_imp = extract_dns_layer(
            pluck_s4(drivers, dnbs_cfg$imp_dens_id) |> stars_obj(),
            dnbs_cfg,
            step
          ),
          crs = dnbs_cfg$crs
        )
      }

      # segmentize path to specified point resolution
      actv_waypnts <- actv_path |>
        sf::st_segmentize(dfMaxLength = dnbs_cfg$waypnts_res) |>
        sf::st_cast("POINT")

      # cumulative length of new path's waypoints
      cum_dist <- sf::st_distance(actv_waypnts, actv_waypnts[1])

      # reset path's total distance travelled by the agent
      total_dist <- units::set_units(0, "m")
      #plot(actv_path, axes = TRUE)
    }

    #print(step)
    #if(step == 178) browser()

    ## Derive energetics (start of current step) ------------------------

    # energy-intake per unit-time given current location. Based on user-provided
    # energy map
    unit_gain <- get_driver_cell_value(pluck_s4(drivers, intake_id), agent)

    # Current energy intake, given state budgets from previous step
    #
    # HACK: this is a temporary solution to convert unit_gain (kJ/h) into actual
    # energy gain (Joules) Several undesirable constraints, e.g. energy gains
    # limited to one state, doesn't fit with the framework structure, etc.
    # Ideally this should be done via a State@energy_gain, analogous to the
    # already existing State@energy_cost. Or maybe the concatenation of the two
    # into a single State@net_energy?
    step_energy_gain <- agent@condition@states_budget[[dnbs_cfg$feed_state_id]] * dnbs_cfg$step_drtn * unit_gain

    # energy-costs per unit-time given current location
    state_unit_costs <- estimate_costs(agent, states_profile, drivers)


    # Current total energy cost, given state budgets from previous step
    #
    # expects two objects to list states in same order. They should, as they
    # were initiated based on same list object during `rmr_initiate()`
    step_state_costs <- purrr::map2(
      state_unit_costs,
      agent@condition@states_budget,
      \(unit_cost, budget) unit_cost * dnbs_cfg$step_drtn * budget
    )

    # Current net total energy
    step_net_energy <- Reduce(`+`, step_state_costs) + step_energy_gain


    ## Rebalance states based on energetic demands at start of the step ------------

    # get night-time fraction at current step
    step_nightprp <- stars::st_extract(night_prop, step_loc, time_column = "tm")[[1]]

    # nudge states budgets based on energetics
    agent@condition@states_budget <- rebalance_states(
      states_budget = agent@condition@states_budget,
      night_prop = step_nightprp,
      feed_state_id = dnbs_cfg$feed_state_id,
      roost_state_id = dnbs_cfg$roost_state_id,
      curr_energy = step_net_energy,
      feed_avg_net_energy = feed_avg_net_energy,
      trgt_energy = units::set_units(1, "kJ"),
      step_duration = dnbs_cfg$step_drtn
    )

    # calculate distance travelled on current path by the end of step, based on
    # states budgets an their speeds

    step_dist <-  purrr::map2(
      agent@properties@speeds,
      agent@condition@states_budget,
      \(speed, budget) speed * budget * dnbs_cfg$step_drtn) |>
      purrr::discard(is.na) |>
      purrr::reduce(sum)

    #step_dist <- units::set_units(15000, "m")

    # path's total distance covered by the end of step
    total_dist <- total_dist + step_dist

    # agent's location at the end of the step
    step_loc <- actv_waypnts[which.min(abs(total_dist - cum_dist))] |>
      st_sf(tm = dnbs_cfg$time_grid[step], geometry = _)


    step_mass_delta <- step_net_energy * agent@properties@energy_to_mass

    # update slots of agents class objects at the end of the step ----------------
    location(agent) <- sf::st_geometry(step_loc)[[1]]
    body_mass(agent) <- max(body_mass(agent) + step_mass_delta, units::set_units(0, "g"))
    agent@condition@mass_change_value <- step_mass_delta
    agent@condition@timestep <- step
    agent@condition@energy_expenditure <- step_net_energy
    agent@condition@states_cost <- state_unit_costs

    hist[[(step + 1)]] <- sf::st_sf(
      timestep = agent@condition@timestep,
      body_mass = body_mass(agent),
      states_budget = agent@condition@states_budget,
      states_unit_cost = agent@condition@states_cost,
      energy_expenditure = agent@condition@energy_expenditure,
      #dist_travelled = step_dist,
      #prop_path = total_dist/max(cum_dist),
      geometry = sf::st_sfc(location(agent))
    )
    #plot(history(agent)["timestep"])
  }

  history(agent) <- do.call(rbind, hist) |>
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

  # get the index of the route corresponding to the input timestep
  route_idx <- which(cfg$routing_timesteps == timestep)

  dns_nrst_dims <- c(cfg$dns_tm_dim, cfg$dns_itr_dim)

  dns_slcs <- list(
    tm = cfg$dns_tm_slices[route_idx],
    itr = if(!is.na(cfg$dns_itr_dim)) sample(cfg$dns_itr_slices, 1) else NULL
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
#' @returns A path
#' @export
#'
#' @examples TBD

calculate_path <- function(agent, dens, crs, impacted = FALSE, imp_dens = NULL) {

  # current location of the agent
  start <- sf::st_sfc(agent@condition@location, crs = crs)

  # generate candidate endpoint based on (un-impacted) density surface
  end <- sample_cell(dens, 1) |>
    sf::st_point() |>
    sf::st_sfc(crs = crs)

  # logic for impacted scenario
  #' #' NOTES:
  #' #' This approach attempts to avoid large deviations in paths between un-impacted
  #' #' and impacted scenarios, by forcing enpoints closer
  if(impacted){

    if(is.null(imp_dens)) stop("imp_dens must be non-null if `impacted`== TRUE")

    # extract endpoint cell value in impacted surface
    # NOTE: experimented with `st_cell()` but, for unclear reasons, the returned
    # cell index appeared to be one pixel short, leading to unexpected outcomes.
    # `stars::st_extract()` is less efficient, but behaves as expected
    imp_end_cell_val <- stars::st_extract(imp_dens, end)[[1]]

    # if cell value is NA relocate endpoint to the closest populated cell in the impacted density surface
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

  if(any(rast_vect < 0)){
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
#' For BioSS analyses
#'
#' @param states_budget A list of time budget allocated to each state (relative proportion)
#' @param night_prop Proportion of day that is night (roost state cannot fall below this)
#' @param feed_state_id ID of the feeding state, providing energy intake
#' @param roost_state_id ID of the roosting state (where night constraint applies)
#' @param curr_energy net energy level at the current time-step (kJ)
#' @param feed_avg_net_energy average net energy intake per unit of time feeding (e.g. kJ/hr)
#' @param trgt_energy cumulative net energy target (kJ)
#' @param step_duration duration of the simulation time-step (e.g 1 day)
#'
#' @returns A list of time budgets allocated to each state (as relative proportions)
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
                             trgt_energy,
                             step_duration){

  out_states <- states_budget

  night_prop <- units::set_units(night_prop, "")

  # lower and upper bounds of feed budget, as proportion of step duration
  feed_upper <- units::set_units(1) - night_prop
  feed_lower <- 0 |> units::set_units(1)

  # net energy demand
  net_target_energy <- trgt_energy - curr_energy

  # feeding duration required to meet energy demand
  feed_hrs <- net_target_energy / feed_avg_net_energy


  # Stage 1 adjustment: meet intake demands -----

  # proportion of time-step required to be spent feeding to meet energy intake demand
  feed_prop <- feed_hrs/step_duration
  feed_prop <- min(feed_prop, feed_upper)
  feed_prop <- max(feed_prop, feed_lower)

  out_states[[feed_state_id]] <- feed_prop

  non_feed_ids <- setdiff(names(states_budget), feed_state_id)

  non_feed_mult <- (units::set_units(1, 1) - out_states[[feed_state_id]]) / Reduce(`+`, states_budget[non_feed_ids])
  out_states[non_feed_ids] <- lapply(states_budget[non_feed_ids], \(x) x * non_feed_mult)


  # Stage 2 adjustment: add roosting restriction (i.e. daily night-length) -----

  if(out_states[[roost_state_id]] < night_prop){

    out_states[[roost_state_id]] <- night_prop

    drop_states <- c(feed_state_id, roost_state_id)
    keep_states <- setdiff(names(states_budget), drop_states)

    remainder_states_mult <- (units::set_units(1, 1) - Reduce(`+`, out_states[drop_states])) / Reduce(`+`, states_budget[keep_states])

    out_states[keep_states] <- lapply(states_budget[keep_states], \(x) x * remainder_states_mult)
  }

  out_states

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






simulate_agent_disnbs_2 <- function(
  agent,
  drivers,
  states_profile,
  scen = c("baseline", "impact"),
  night_proportion,
  dnbs_cfg,
  feed_avg_net_energy,
  target_energy = units::set_units(1, "kJ")
) {
  # TODO
  # - Improve calculation of step travelled distance based on a correlated random
  #   walk
  # - nudging final positions once end of track is reached
  # - further factorisation?

  # NOTES:
  #  - derivation of agent locations on tracks: sf::st_line_interpolate() ended up being favoured over previous waypoints logic, for it improves accuracy and code cleanliness, without no impact on computational efficiency.

  # check ------------------------------------------------------
  if (!inherits(dnbs_cfg, "disnbs_config")) {
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
  if (scen == "baseline") {
    impacted <- FALSE
    intake_id <- dnbs_cfg$intake_id
  } else {
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
    \(s) {
      units(s) <- NULL
      s
    }
  )

  feed_avg_net_energy <- assert_units_to_numeric(feed_avg_net_energy, "kJ/h")
  target_energy <- assert_units_to_numeric(target_energy, "kJ")
  energy_to_mass <- assert_units_to_numeric(
    agent@properties@energy_to_mass,
    "g/kJ"
  )

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
  track_travelled <- 0
  track_length <- 0

  # initiate agent's history
  # NOTE: currently overwrites object from Agent's initialization step in
  # rmr_initiate()
  hist_0 <- sf::st_sf(
    timestep = 0L,
    timestamp = as.POSIXct(NA),
    track_id = track_id,
    body_mass = body_mass(agent),
    states_budget = agent@condition@states_budget,
    states_unit_cost = agent@condition@states_cost,
    energy_expenditure = agent@condition@energy_expenditure,
    geometry = sf::st_sfc(location(agent))
  )

  # initial location as sf
  step_loc <- sf::st_sf(
    tm = dnbs_cfg$time_grid[1],
    geometry = sf::st_sfc(location(agent), crs = dnbs_cfg$crs)
  )

  # run -----------------------------------------------------
  # for-loop over simulations timepoints
  for (step in seq_along(dnbs_cfg$time_grid)) {
    ## Generate track ---------------------------------------------------------
    # done at "start of the day", i.e. before moving the agent on current delta_time

    # Evaluate re-routing needs.
    reroute <- if (step == 1L || dnbs_cfg$dns_routing[step]) {
      TRUE
    } else {
      # check if agent has reached the end of the current track, when the total distance travelled on track exceeds the track's total length.
      track_travelled >= track_length
    }

    if (reroute) {
      dens <- extract_dns_layer(
        pluck_s4(drivers, dnbs_cfg$dens_id) |> stars_obj(),
        dnbs_cfg,
        step
      )

      # generate new track
      actv_track <- if (!impacted) {
        calculate_track(
          agent,
          dens = dens,
          impacted = FALSE,
          crs = dnbs_cfg$crs,
          aoc_bbx = dnbs_cfg$aoc_bbx
        )
      } else {
        # impacted agent
        imp_dens <- extract_dns_layer(
          pluck_s4(drivers, dnbs_cfg$imp_dens_id) |> stars_obj(),
          dnbs_cfg,
          step
        )

        # guard against agents with starting locations on "empty" cells of
        # the impacted surface
        if (step == 1L) {
          location(agent) <- snap_to_nearest_pop_cell(
            sf::st_sfc(location(agent), crs = dnbs_cfg$crs),
            imp_dens
          )[[1]]
        }

        calculate_track(
          agent,
          dens = dens,
          impacted = TRUE,
          imp_dens = imp_dens,
          crs = dnbs_cfg$crs,
          aoc_bbx = dnbs_cfg$aoc_bbx
        )
      }

      # safeguard on `actv_track` being an empty linestring - set it to agent's current location (POINT)
      if (sf::st_is_empty(actv_track)) {
        actv_track <- sf::st_sfc(
          sf::st_point(location(agent)),
          crs = dnbs_cfg$crs
        )
      }

      # track length in meters, as numeric
      track_length <- sf::st_length(actv_track) |>
        assert_units_to_numeric("meters")

      # reset track's total distance travelled by the agent (meters)
      track_travelled <- 0
      #plot(actv_track, axes = TRUE)
      track_id <- track_id + 1L
    }

    ## * Derive energetics ---------------------------------------------------
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
      lapply(function(s) assert_units_to_numeric(s, "kJ/h"))

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
    night_prop <- stars::st_extract(
      night_proportion,
      step_loc,
      time_column = "tm"
    )[[1]]

    # if (is.na(net_energy)) {
    #   browser()
    # }
    # if (step >= 6) {
    #   browser()
    # }
    # if (
    #   (dnbs_cfg$time_grid[step] >= as.Date("2023-02-01")) && reroute == TRUE
    # ) {
    #   browser()
    # }

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
    step_dist <- purrr::map2(
      agent_speeds,
      states_budget(agent@condition),
      \(speed, budget) speed * budget * step_drtn_hrs
    ) |>
      purrr::discard(is.na) |>
      purrr::reduce(sum)

    # track's total distance covered by the end of step (meters)
    track_travelled <- track_travelled + step_dist

    # proportion of track covered, to use in interpolation of agent's location on track
    track_coverage <- units::set_units(
      track_travelled / track_length,
      dnbs_cfg$crs$units_gdal,
      mode = "standard"
    )

    # interpolate agent's location on track at the end of the step
    suppressMessages(
      pnt <- sf::st_line_interpolate(
        actv_track,
        track_coverage,
        normalized = TRUE
      )
    )

    ## Update Agent slots ------------------------------------------
    # (at the end of the step)

    location(agent) <- sf::st_geometry(step_loc)[[1]]
    agent@condition@timestep <- step
    agent@condition@timestamp <- as.POSIXct(dnbs_cfg$time_grid[step], "UTC")
    agent@condition@energy_expenditure <- units::set_units(net_energy, "kJ")
    agent@condition@states_cost <- lapply(
      state_unit_costs,
      units::set_units,
      "kJ/h"
    )

    step_mass_delta <- units::set_units(step_mass_delta, "g")
    agent@condition@mass_change_value <- step_mass_delta
    body_mass(agent) <- agent@properties@initial_mass + step_mass_delta

    hist[[step]] <- sf::st_sf(
      timestep = agent@condition@timestep,
      timestamp = agent@condition@timestamp,
      track_id = track_id,
      body_mass = body_mass(agent),
      states_budget = agent@condition@states_budget,
      states_unit_cost = agent@condition@states_cost,
      energy_expenditure = agent@condition@energy_expenditure,
      geometry = sf::st_sfc(location(agent))
    )
    #plot(history(agent)["timestep"])
  }

  # handle history ----------------------------------------
  hist <- do.call(rbind, hist)

  if (isTRUE(dnbs_cfg$bm_smooth$apply)) {
    hist <- hist |>
      dplyr::mutate(
        body_mass_smooth = smooth_body_mass(
          timestep,
          body_mass,
          dnbs_cfg$bm_smooth$ks_bw
        ),
        .after = body_mass
      )
  }

  history(agent) <- dplyr::add_row(hist, hist_0, .before = 1) |>
    sf::st_set_crs(dnbs_cfg$crs)

  agent
}

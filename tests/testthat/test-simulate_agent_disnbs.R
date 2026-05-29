# main function testing -----------
test_that("dev testing", {
  skip("developing purposes only")

  conf <- create_dnbs_config(
    rover_ibm_disnbs@drivers[[1]],
    rover_ibm_disnbs@model_config,
    ids = list(
      dens_id = "dens",
      intake_id = "intake",
      imp_dens_id = "imp_dens",
      imp_intake_id = "imp_intake",
      feed_state_id = "foraging",
      roost_state_id = "water_resting"
    ),
    waypnts_res = 2000,
    bmsm_opts = bm_smooth_opts("7 days")
  )

  npr <- derive_night_cube(
    strs = rover_ibm_disnbs@drivers[[1]] |> stars_obj(),
    start_date = rover_ibm_disnbs@model_config@start_date,
    end_date = rover_ibm_disnbs@model_config@end_date,
    delta_time = "1 week"
  )

  set.seed(1000)
  a <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[1]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "baseline",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  a <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[1]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "impact",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  withr::local_package("ggplot2")
  withr::local_package("units")

  history(a) |>
    ggplot(aes(x = timestep)) +
    geom_point(aes(y = body_mass, col = factor(track_id))) +
    geom_line(aes(y = body_mass_smooth))

  plot(history(a)["timestep"], pch = 19)
  plot(history(a)["path_id"], pch = 19)

  bench::mark(
    iterations = 25,
    a = {
      set.seed(1234)
      simulate_agent_disnbs(
        agent = rover_ibm_disnbs@agents[[1]],
        drivers = rover_ibm_disnbs@drivers,
        states_profile = rover_ibm_disnbs@species@states_profile,
        scen = "baseline",
        night_prop = npr,
        dnbs_cfg = conf,
        feed_avg_net_energy = units::set_units(422, "kJ/h")
      )
    }
  )

  set.seed(1992)
  agents_bsln <- purrr::map(
    rover_ibm_disnbs@agents[1:40],
    function(a) {
      simulate_agent_disnbs(
        agent = a,
        drivers = rover_ibm_disnbs@drivers,
        states_profile = rover_ibm_disnbs@species@states_profile,
        scen = "baseline",
        night_proportion = npr,
        dnbs_cfg = conf,
        feed_avg_net_energy = units::set_units(422, "kJ/h"),
        target_energy = units::set_units(1, "kJ")
      )
    },
    .progress = list(
      format = "{cli::pb_bar} {cli::pb_percent} [{cli::pb_elapsed}] | ETA: {cli::pb_eta}"
    )
  )
})


test_that("simulate_agent_disnbs(): matched-pairs work as expected movement-wise", {
  withr::local_package("ggplot2")
  withr::local_package("vdiffr")

  # required inputs
  set.seed(129)
  conf <- create_dnbs_config(
    dens_drv = pluck_s4(rover_ibm_disnbs@drivers, "dens"),
    rover_ibm_disnbs@model_config,
    ids = list(
      dens_id = "dens",
      intake_id = "intake",
      imp_dens_id = "imp_dens",
      imp_intake_id = "imp_intake",
      feed_state_id = "foraging",
      roost_state_id = "water_resting"
    ),
    bmsm_opts = bm_smooth_opts("7 days"),
    waypnts_res = 2000
  )

  npr <- derive_night_cube(
    strs = pluck_s4(rover_ibm_disnbs@drivers, "dens") |> stars_obj(),
    start_date = rover_ibm_disnbs@model_config@start_date,
    end_date = rover_ibm_disnbs@model_config@end_date,
    delta_time = "1 week"
  )

  # Baseline Vs. impacted runs - non-impacted Agent
  # Locations should be similar between scenarios, but deviations can occur due to diverging energetics (and hence travelled distances) from distinct underpinning intake maps. These will manifested in terms of timestep locations along tracks, or distinct tracks when agents reach the end of track faster under one of the scenarios, rerouting in a different timestep (and hence potentially a different density layer) than the other scenario.
  unimpacted_idx <- which(
    purrr::map_lgl(
      rover_ibm_disnbs@agents,
      ~ .x@properties@move_influences$imp_dens$infl
    ) ==
      FALSE
  )

  set.seed(11002)
  #set.seed(1004)
  a_bsln <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[unimpacted_idx[4]]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "baseline",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  set.seed(11002)
  #set.seed(1004)
  a_imp <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[unimpacted_idx[4]]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "impact",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  a_bsln_hist <- history(a_bsln) |>
    dplyr::slice(-1) |>
    dplyr::mutate(
      timepoint = conf$time_grid,
      months = lubridate::month(timepoint, label = TRUE, abbr = TRUE) |>
        factor(ordered = FALSE),
      geom_lag = dplyr::lag(geometry),
      scenario = "baseline"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      track_segment = st_cast(
        st_union(geometry, geom_lag),
        "LINESTRING"
      )
    ) |>
    dplyr::ungroup()

  a_imp_hist <- history(a_imp) |>
    dplyr::slice(-1) |>
    dplyr::mutate(
      timepoint = conf$time_grid,
      months = lubridate::month(timepoint, label = TRUE, abbr = TRUE) |>
        factor(ordered = FALSE),
      geom_lag = dplyr::lag(geometry),
      scenario = "impact"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      track_segment = st_cast(
        st_union(geometry, geom_lag),
        "LINESTRING"
      )
    ) |>
    dplyr::ungroup()

  a_hist <- rbind(a_bsln_hist, a_imp_hist)

  p_unimp <- ggplot() +
    theme_void() +
    stars::geom_stars(
      data = pluck_s4(rover_ibm_disnbs@drivers, "imp_dens") |>
        stars_obj() |>
        dplyr::slice(iter, 3)
    ) +
    geom_sf(data = a_hist, aes(col = scenario), size = 2) +
    geom_sf(
      data = a_hist |> sf::st_set_geometry("track_segment"),
      aes(col = scenario),
      linetype = "dotted"
    ) +
    scale_fill_viridis_c(transform = "sqrt", option = "B") +
    scale_color_brewer(palette = "Set1", direction = -1) +
    facet_wrap(vars(months))

  expect_doppelganger("sim_matched_runs_move_unimpacted", p_unimp)

  # Baseline Vs. impacted runs - impacted Agent: tracks should strongly differ if the
  # shortest path on baseline scenario crosses a footprint
  impacted_idx <- which(
    purrr::map_lgl(
      rover_ibm_disnbs@agents,
      ~ .x@properties@move_influences$imp_dens$infl
    ) ==
      TRUE
  )

  set.seed(1000)
  a_bsln <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[impacted_idx[10]]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "baseline",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  set.seed(1000)
  a_imp <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[impacted_idx[10]]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "impact",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  a_bsln_hist <- history(a_bsln) |>
    dplyr::slice(-1) |>
    dplyr::mutate(
      timepoint = conf$time_grid,
      months = lubridate::month(timepoint, label = TRUE, abbr = TRUE) |>
        factor(ordered = FALSE),
      geom_lag = dplyr::lag(geometry),
      scenario = "baseline"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      track_segment = st_cast(
        st_union(geometry, geom_lag),
        "LINESTRING"
      )
    ) |>
    dplyr::ungroup()

  a_imp_hist <- history(a_imp) |>
    dplyr::slice(-1) |>
    dplyr::mutate(
      timepoint = conf$time_grid,
      months = lubridate::month(timepoint, label = TRUE, abbr = TRUE) |>
        factor(ordered = FALSE),
      geom_lag = dplyr::lag(geometry),
      scenario = "impact"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      track_segment = st_cast(
        st_union(geometry, geom_lag),
        "LINESTRING"
      )
    ) |>
    dplyr::ungroup()

  a_hist <- rbind(a_bsln_hist, a_imp_hist)

  p_imp <- ggplot() +
    theme_void() +
    stars::geom_stars(
      data = pluck_s4(rover_ibm_disnbs@drivers, "imp_dens") |>
        stars_obj() |>
        dplyr::slice(iter, 3)
    ) +
    geom_sf(data = a_hist, aes(col = scenario), size = 2) +
    geom_sf(
      data = a_hist |> sf::st_set_geometry("track_segment"),
      aes(col = scenario),
      linetype = "dotted"
    ) +
    scale_fill_viridis_c(transform = "sqrt", option = "B") +
    scale_color_brewer(palette = "Set1", direction = -1) +
    facet_wrap(vars(months))

  expect_doppelganger("sim_matched_runs_move_impacted", p_imp)
})


test_that("simulate_agent_disnbs(): matched-runs work as expected condition-wise", {
  withr::local_package("ggplot2")
  withr::local_package("vdiffr")
  withr::local_package("patchwork")
  withr::local_package("units")

  # required inputs
  set.seed(129)
  conf <- create_dnbs_config(
    dens_drv = pluck_s4(rover_ibm_disnbs@drivers, "dens"),
    rover_ibm_disnbs@model_config,
    ids = list(
      dens_id = "dens",
      intake_id = "intake",
      imp_dens_id = "imp_dens",
      imp_intake_id = "imp_intake",
      feed_state_id = "foraging",
      roost_state_id = "water_resting"
    ),
    bmsm_opts = bm_smooth_opts("7 days"),
    waypnts_res = 2000
  )

  npr <- derive_night_cube(
    strs = pluck_s4(rover_ibm_disnbs@drivers, "dens") |> stars_obj(),
    start_date = rover_ibm_disnbs@model_config@start_date,
    end_date = rover_ibm_disnbs@model_config@end_date,
    delta_time = "1 week"
  )

  # Baseline Vs. impacted runs - non-impacted Agent:
  unimpacted_idx <- which(
    purrr::map_lgl(
      rover_ibm_disnbs@agents,
      ~ .x@properties@move_influences$imp_dens$infl
    ) ==
      FALSE
  )

  set.seed(11002)
  a_bsln <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[unimpacted_idx[4]]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "baseline",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  set.seed(11002)
  a_imp <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[unimpacted_idx[4]]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "impact",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  output <- dplyr::bind_rows(
    list(bsln = history(a_bsln), imp = history(a_imp)),
    .id = "scen"
  )

  p_unimp_bm <- output |>
    dplyr::filter(timestep != 0) |>
    ggplot() +
    theme_minimal() +
    geom_line(
      aes(x = timestep, y = body_mass_smooth, colour = scen),
      linewidth = 2
    ) +
    scale_color_brewer(palette = "Set1", direction = -1)

  p_unimp_net_energy <- output |>
    dplyr::filter(timestep != 0) |>
    ggplot() +
    theme_minimal() +
    geom_line(
      aes(x = timestep, y = energy_expenditure, colour = scen),
      linewidth = 2
    ) +
    scale_color_brewer(palette = "Set1", direction = -1)

  p_unimp <- p_unimp_bm +
    p_unimp_net_energy +
    plot_layout(guides = 'collect') +
    plot_annotation(title = "Unimpacted Agent")
  # p_unimp

  suppressWarnings(
    expect_doppelganger(
      "sim_matched_runs_bodymass_unimpacted_agent",
      p_unimp
    )
  )

  # Baseline Vs. impacted runs - impacted Agent:
  impacted_idx <- which(
    purrr::map_lgl(
      rover_ibm_disnbs@agents,
      ~ .x@properties@move_influences$imp_dens$infl
    ) ==
      TRUE
  )

  set.seed(1979)
  a_bsln <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[impacted_idx[10]]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "baseline",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  set.seed(1979)
  a_imp <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[impacted_idx[10]]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "impact",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  output <- dplyr::bind_rows(
    list(bsln = history(a_bsln), imp = history(a_imp)),
    .id = "scen"
  )

  p_imp_bm <- output |>
    dplyr::filter(timestep != 0) |>
    ggplot() +
    theme_minimal() +
    geom_line(
      aes(x = timestep, y = body_mass_smooth, colour = scen),
      linewidth = 2
    ) +
    scale_color_brewer(palette = "Set1", direction = -1)

  p_imp_net_energy <- output |>
    dplyr::filter(timestep != 0) |>
    ggplot() +
    theme_minimal() +
    geom_line(
      aes(x = timestep, y = energy_expenditure, colour = scen),
      linewidth = 2
    ) +
    scale_color_brewer(palette = "Set1", direction = -1)

  p_imp <- p_imp_bm +
    p_imp_net_energy +
    plot_layout(guides = 'collect') +
    plot_annotation(title = "Impacted Agent")
  # p_imp

  suppressWarnings(
    expect_doppelganger("sim_matched_runs_bodymass_impacted_agent", p_imp)
  )
})


test_that("simulate_agent_disnbs() handles starting loc on empty cell of impacted density", {
  # required inputs
  set.seed(129)
  conf <- create_dnbs_config(
    dens_drv = pluck_s4(rover_ibm_disnbs@drivers, "dens"),
    rover_ibm_disnbs@model_config,
    ids = list(
      dens_id = "dens",
      intake_id = "intake",
      imp_dens_id = "imp_dens",
      imp_intake_id = "imp_intake",
      feed_state_id = "foraging",
      roost_state_id = "water_resting"
    ),
    bmsm_opts = bm_smooth_opts("7 days"),
    waypnts_res = 2000
  )

  npr <- derive_night_cube(
    strs = pluck_s4(rover_ibm_disnbs@drivers, "dens") |> stars_obj(),
    start_date = rover_ibm_disnbs@model_config@start_date,
    end_date = rover_ibm_disnbs@model_config@end_date,
    delta_time = "1 week"
  )

  impacted_idx <- which(
    purrr::map_lgl(
      rover_ibm_disnbs@agents,
      ~ .x@properties@move_influences$imp_dens$infl
    ) ==
      TRUE
  )

  # isolate an impacted agent
  a <- rover_ibm_disnbs@agents[[impacted_idx[10]]]

  # find empty cell in impact density and assign it to agent's starting location
  d_imp <- rover_ibm_disnbs@drivers[[3]]@stars_obj[,,, 1, 1, drop = TRUE]
  empty_cell_coords <- st_coordinates(d_imp)[
    which(is.na(d_imp[[1]]))[1],
    c("x", "y")
  ]
  empty_cell_pnt <- st_sfc(
    st_point(as.matrix(empty_cell_coords)),
    crs = st_crs(conf$crs)
  )
  location(a) <- empty_cell_pnt[[1]]

  # No error means issue is handled appropriately
  expect_no_error(
    out <- simulate_agent_disnbs(
      agent = a,
      drivers = rover_ibm_disnbs@drivers,
      states_profile = rover_ibm_disnbs@species@states_profile,
      scen = "impact",
      night_prop = npr,
      dnbs_cfg = conf,
      feed_avg_net_energy = units::set_units(422, "kJ/h"),
      target_energy = units::set_units(1, "kJ")
    )
  )
})


test_that("simulate_agent_disnbs() fails when inputs are invalid", {
  # set up
  conf <- create_dnbs_config(
    dens_drv = pluck_s4(rover_ibm_disnbs@drivers, "dens"),
    rover_ibm_disnbs@model_config,
    ids = list(
      dens_id = "dens",
      intake_id = "intake",
      imp_dens_id = "imp_dens",
      imp_intake_id = "imp_intake",
      feed_state_id = "foraging",
      roost_state_id = "water_resting"
    ),
    bmsm_opts = bm_smooth_opts("7 days"),
    waypnts_res = 2000
  )

  npr <- derive_night_cube(
    strs = pluck_s4(rover_ibm_disnbs@drivers, "dens") |> stars_obj(),
    start_date = rover_ibm_disnbs@model_config@start_date,
    end_date = rover_ibm_disnbs@model_config@end_date,
    delta_time = "1 week"
  )

  d <- rover_ibm_disnbs@drivers
  a <- rover_ibm_disnbs@agents[[9]]
  s <- rover_ibm_disnbs@species@states_profile
  fane <- units::set_units(422, "kJ/h")

  # wrong `dnbs_cfg`
  cfg <- "blalala"
  expect_snapshot(
    simulate_agent_disnbs(a, d, s, "impact", npr, dnbs_cfg = cfg, fane),
    error = TRUE
  )

  # missing impacted drivers IDs when `impact = TRUE`
  cfg <- conf
  cfg$imp_dens_id <- c(NULL)

  expect_snapshot(
    simulate_agent_disnbs(a, d, s, "impact", npr, cfg, fane),
    error = TRUE
  )

  cfg <- conf
  cfg$imp_intake_id <- c(NULL)
  expect_snapshot(
    simulate_agent_disnbs(a, d, s, "impact", npr, cfg, fane),
    error = TRUE
  )
})


test_that("simulate_agent_disnbs(): routing works as expected", {
  withr::local_package("ggplot2")
  withr::local_package("vdiffr")
  withr::local_package("lwgeom")

  npr <- derive_night_cube(
    strs = pluck_s4(rover_ibm_disnbs@drivers, "dens") |> stars_obj(),
    start_date = rover_ibm_disnbs@model_config@start_date,
    end_date = rover_ibm_disnbs@model_config@end_date,
    delta_time = "1 week"
  )

  set.seed(129)
  conf <- create_dnbs_config(
    dens_drv = pluck_s4(rover_ibm_disnbs@drivers, "dens"),
    rover_ibm_disnbs@model_config,
    ids = list(
      dens_id = "dens",
      intake_id = "intake",
      imp_dens_id = "imp_dens",
      imp_intake_id = "imp_intake",
      feed_state_id = "foraging",
      roost_state_id = "water_resting"
    ),
    bmsm_opts = bm_smooth_opts("7 days"),
    waypnts_res = 2000
  )

  impacted_idx <- which(
    purrr::map_lgl(
      rover_ibm_disnbs@agents,
      ~ .x@properties@move_influences$imp_dens$infl
    ) ==
      TRUE
  )

  set.seed(1000)
  a_imp <- simulate_agent_disnbs(
    agent = rover_ibm_disnbs@agents[[impacted_idx[4]]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "impact",
    night_prop = npr,
    dnbs_cfg = conf,
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ")
  )

  a_imp_hist <- history(a_imp) |>
    dplyr::slice(-1) |>
    dplyr::mutate(
      timepoint = conf$time_grid,
      months = lubridate::month(timepoint, label = TRUE, abbr = TRUE) |>
        factor(ordered = FALSE)
    )

  a_imp_hist <- a_imp_hist |>
    dplyr::mutate(geom_lag = dplyr::lag(geometry)) |>
    dplyr::mutate(
      month_track = 1:dplyr::n(),
      .by = c(months)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      track_segment = st_cast(st_union(geometry, geom_lag), "LINESTRING")
    ) |>
    dplyr::ungroup()

  # convert bearing degrees to radians degrees anti-clockwise from east and ensure it's between 0 and 2*pi
  a_imp_hist$bearing <- c(as.numeric(lwgeom::st_geod_azimuth(a_imp_hist)), NA)
  a_imp_hist$bearing <- (pi / 2 - a_imp_hist$bearing) %% (2 * pi)

  p <- ggplot() +
    theme_void() +
    stars::geom_stars(
      data = pluck_s4(rover_ibm_disnbs@drivers, "imp_dens") |>
        stars_obj() |>
        dplyr::slice(iter, 3) #|>
      #dplyr::slice(months, "Jan")
    ) +
    geom_sf(
      #data = dplyr::filter(a_imp_hist, months == "Jan"),
      data = a_imp_hist,
      aes(colour = factor(month_track))
      #aes(colour = timestep),
      #col = "white"
    ) +
    # add timestep to plot to visualise track segments
    geom_sf_label(
      data = dplyr::filter(a_imp_hist, months == "Jan"),
      aes(label = timestep, colour = factor(track_id)),
      size = 5,
      nudge_y = 0.08,
      #nudge_x = 0.1,
      fontface = "bold"
    ) +
    geom_spoke(
      #data = dplyr::filter(a_imp_hist, months == "Jan"),
      data = dplyr::filter(a_imp_hist),
      aes(
        x = st_coordinates(geometry)[, 1],
        y = st_coordinates(geometry)[, 2],
        angle = as.numeric(bearing),
        radius = 0.1,
        colour = factor(month_track)
      ),
      #colour = "white",
      arrow = arrow(length = unit(0.1, "inches"), type = "open")
    ) +
    geom_sf(
      data = a_imp_hist |> # dplyr::filter(a_imp_hist, months == "Jan") |>
        sf::st_set_geometry("track_segment"),
      #colour = "white",
      aes(colour = factor(month_track)),
      linetype = "dotted",
      #arrow = arrow(length = unit(0.1, "inches"), type = "closed")
    ) +
    scale_fill_viridis_c(transform = "sqrt", option = "B") +
    guides(colour = "none") +
    facet_wrap(~months)

  suppressWarnings(
    expect_doppelganger("sim_agent_disnbs_routing", p)
  )
})

# helpers testing --------------------------

test_that("calculate_track() works as expected", {
  withr::local_package("ggplot2")
  withr::local_package("vdiffr")

  #calculate_track(agent, dens, crs, impacted = FALSE, dens_imp = NULL)

  d <- rover_ibm_disnbs@drivers |>
    purrr::keep(\(d) d@id == "dens") |>
    purrr::pluck(1) |>
    stars_obj() |>
    dplyr::slice(months, 4) |>
    dplyr::slice(iter, 1)

  imp_d <- rover_ibm_disnbs@drivers |>
    purrr::keep(\(d) d@id == "imp_dens") |>
    purrr::pluck(1) |>
    stars_obj() |>
    dplyr::slice(months, 4) |>
    dplyr::slice(iter, 1)

  # Impact doesn't affect path, so identical un-impacted and impacted paths
  set.seed(191)
  pth <- calculate_track(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    crs = rover_ibm_disnbs@model_config@ref_sys,
    aoc_bbx = rover_ibm_disnbs@model_config@aoc_bbx
  )

  set.seed(191)
  imp_pth <- calculate_track(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    impacted = TRUE,
    imp_dens = imp_d,
    crs = rover_ibm_disnbs@model_config@ref_sys,
    aoc_bbx = rover_ibm_disnbs@model_config@aoc_bbx
  )

  p <- ggplot() +
    stars::geom_stars(data = imp_d) +
    geom_sf(data = pth, col = "green", linewidth = 1.2) +
    geom_sf(data = imp_pth, col = "red", linewidth = 1.2)

  expect_doppelganger("identical_imp-unimp", p)

  # different endpoints and diverging paths
  set.seed(19199)
  pth <- calculate_track(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    crs = rover_ibm_disnbs@model_config@ref_sys,
    aoc_bbx = rover_ibm_disnbs@model_config@aoc_bbx
  )

  set.seed(19199)
  imp_pth <- calculate_track(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    impacted = TRUE,
    imp_dens = imp_d,
    crs = rover_ibm_disnbs@model_config@ref_sys,
    aoc_bbx = rover_ibm_disnbs@model_config@aoc_bbx
  )

  p <- ggplot() +
    stars::geom_stars(data = imp_d) +
    geom_sf(data = pth, col = "green", linewidth = 1.2) +
    geom_sf(data = imp_pth, col = "red", linewidth = 1.2)

  expect_doppelganger("diff_endpoints", p)

  # same end point but impacted path diverge to avoid hole
  set.seed(23)
  pth <- calculate_track(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    crs = rover_ibm_disnbs@model_config@ref_sys,
    aoc_bbx = rover_ibm_disnbs@model_config@aoc_bbx
  )

  set.seed(23)
  imp_pth <- calculate_track(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    impacted = TRUE,
    imp_dens = imp_d,
    crs = rover_ibm_disnbs@model_config@ref_sys,
    aoc_bbx = rover_ibm_disnbs@model_config@aoc_bbx
  )

  p <- ggplot() +
    stars::geom_stars(data = imp_d) +
    geom_sf(data = pth, col = "green", linewidth = 1.2) +
    geom_sf(data = imp_pth, col = "red", linewidth = 1.2)

  expect_doppelganger("same_endpoint_diff_path", p)
})


test_that("extract_dns_layer() works as expected", {
  # dens with temp and iter dimensions
  step <- 3

  conf <- create_dnbs_config(
    rover_ibm_disnbs@drivers[[1]],
    rover_ibm_disnbs@model_config,
    ids = list()
  )

  out <- extract_dns_layer(
    dns_strs = rover_ibm_disnbs@drivers[[1]] |> stars_obj(),
    conf,
    timestep = step
  )

  expected <- rover_ibm_disnbs@drivers[[1]] |>
    stars_obj() |>
    dplyr::slice(months, conf$dns_tm_slices[step]) |>
    dplyr::slice(iter, conf$dns_itr_slices[step])

  expect_equal(out, expected)
  expect_length(dim(out), 2)

  # dens with only temporal dimension
  d <- Driver(
    "t",
    stars_obj = rover_ibm_disnbs@drivers[[1]] |>
      stars_obj() |>
      dplyr::slice(iter, 2)
  )
  conf <- create_dnbs_config(d, rover_ibm_disnbs@model_config, id = list())

  step <- 2
  out <- extract_dns_layer(
    timestep = step,
    dns_strs = stars_obj(d),
    conf
  )
  expected <- stars_obj(d) |>
    dplyr::slice(months, conf$dns_tm_slices[step])

  expect_equal(out, expected)
  expect_length(dim(out), 2)

  # dens with only iteration dimension
  d <- Driver(
    "t",
    stars_obj = rover_ibm_disnbs@drivers[[1]] |>
      stars_obj() |>
      dplyr::slice(months, 2)
  )
  conf <- create_dnbs_config(d, rover_ibm_disnbs@model_config, id = list())

  out <- extract_dns_layer(
    timestep = step,
    dns_strs = stars_obj(d),
    conf
  )

  expected <- stars_obj(d) |>
    dplyr::slice(iter, conf$dns_itr_slices[step])

  expect_equal(out, expected)
  expect_length(dim(out), 2)

  # raster-only density (i.e no temporal nor iteration dimensions)
  d <- Driver("t", stars_obj = generate_mock_stars(attr_units = "g"))
  conf <- create_dnbs_config(d, rover_ibm_disnbs@model_config, ids = list())

  out <- extract_dns_layer(
    timestep = step,
    dns_strs = stars_obj(d),
    conf
  )
  expected <- stars_obj(d)

  expect_equal(out, expected)
  expect_length(dim(out), 2)
})


test_that("sample_cell() works as expected", {
  # force specific cell to be picked
  x <- generate_mock_stars(attr_units = "g") * 0
  x[[1]][4, 2] <- 3

  pnt <- sample_cell(x, 1)

  expect_equal(as.double(stars::st_extract(x, pnt)), 3)

  # fails if attribute values are negative, as doesn't comply with the
  # derivation of PDF
  expect_error(
    {
      x <- generate_mock_stars() * -1
      sample_cell(x)
    },
    "All values in the first attribute of `x` must be non-negative."
  )
})


test_that("nudge_pnt_into_bbox() works as expected", {
  #skip()

  # bbox with all positive limits ----------
  bbox <- create_bbox(0, 0, 5, 5)

  # point inside: out == in
  loc <- sf::st_sfc(sf::st_point(c(1, 1)))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_equal(nudged_loc, loc)
  # plot(sf::st_as_sfc(bbox))
  # plot(loc, add = TRUE, col = "red", lwd = 2, cex = 1.2)
  # plot(nudged_loc, add = TRUE, col = "green", pch = 19)

  loc <- sf::st_sf(a = 1, geom = sf::st_sfc(sf::st_point(c(1, 1))))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_equal(nudged_loc, loc)

  # point outside: in nudged inside
  loc <- sf::st_sfc(sf::st_point(c(-1, 7)))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_true(sf::st_intersects(
    sf::st_as_sfc(bbox),
    nudged_loc,
    sparse = FALSE
  ))

  # plot(sf::st_as_sfc(bbox), xlim = c(-2, 8), ylim = c(-2, 8))
  # plot(loc, add = TRUE, col = "red", lwd = 2, cex = 1.2)
  # plot(nudged_loc, add = TRUE, col = "green", pch = 19)

  # bbox with partially negative limits ----------
  bbox <- create_bbox(-5, -3, 10, 10)

  # point inside: out == in
  loc <- sf::st_sfc(sf::st_point(c(-1, 7)))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_equal(nudged_loc, loc)

  # plot(sf::st_as_sfc(bbox), xlim = c(-10, 12), ylim = c(-4, 12))
  # plot(loc, add = TRUE, col = "red", lwd = 2, cex = 1.2)
  # plot(nudged_loc, add = TRUE, col = "green", pch = 19)

  # point outside: in nudged inside
  loc <- sf::st_sfc(sf::st_point(c(11, -4)))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_true(sf::st_intersects(
    sf::st_as_sfc(bbox),
    nudged_loc,
    sparse = FALSE
  ))
  # plot(sf::st_as_sfc(bbox), xlim = c(-10, 12), ylim = c(-4, 12))
  # plot(loc, add = TRUE, col = "red", lwd = 2, cex = 1.2)
  # plot(nudged_loc, add = TRUE, col = "green", pch = 19)

  loc <- sf::st_sfc(sf::st_point(c(11, 14)))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_true(sf::st_intersects(
    sf::st_as_sfc(bbox),
    nudged_loc,
    sparse = FALSE
  ))

  loc <- sf::st_sfc(sf::st_point(c(-10, -10)))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_true(sf::st_intersects(
    sf::st_as_sfc(bbox),
    nudged_loc,
    sparse = FALSE
  ))

  # bbox with all negative limits ----------
  bbox <- create_bbox(-5, -5, -1, -1)

  # point inside: out == in
  loc <- sf::st_sfc(sf::st_point(c(-1.1, -1.5)))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_equal(nudged_loc, loc)

  # point outside: nudged inside
  loc <- sf::st_sfc(sf::st_point(c(11, -4)))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_true(sf::st_intersects(
    sf::st_as_sfc(bbox),
    nudged_loc,
    sparse = FALSE
  ))

  loc <- sf::st_sfc(sf::st_point(c(11, 14)))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_true(sf::st_intersects(
    sf::st_as_sfc(bbox),
    nudged_loc,
    sparse = FALSE
  ))

  loc <- sf::st_sfc(sf::st_point(c(-10, -10)))
  nudged_loc <- nudge_pnt_into_bbox(loc, bbox)
  expect_true(sf::st_intersects(
    sf::st_as_sfc(bbox),
    nudged_loc,
    sparse = FALSE
  ))
})


test_that("snap_to_nearest_pop_cell() works as expected", {
  withr::local_package("ggplot2")
  withr::local_package("vdiffr")

  # simulate stars object
  m <- matrix(NA_real_, 100, 100)
  # Two populated "islands"
  set.seed(1991)
  m[10:40, 10:40] <- runif(31 * 31, 0, 1)
  m[60:90, 60:90] <- runif(31 * 31, 0, 1)
  d <- stars::st_as_stars(m) |> setNames("value")

  # A point that falls on NA, so relocation is needed
  p_orig <- sf::st_sfc(sf::st_point(c(49, 49)))

  # check that surface evaluates to NA at point
  expect_true(is.na(stars::st_extract(d, p_orig)[["value"]]))

  # perform snap
  p_moved <- snap_to_nearest_pop_cell(p_orig, d)

  # cell value at snapped point is non-NA
  expect_true(!is.na(stars::st_extract(d, p_moved)[["value"]]))

  # Test on multiple points ----

  # random sample of 200 points falling in NA cells
  na_cells_idx <- which(is.na(d[[1]]), arr.ind = TRUE)
  na_cells <- na_cells_idx[sample(nrow(na_cells_idx), 200), ] - 0.5 # 0.5 take-way to match cell centres

  pts_orig <- na_cells |>
    apply(1, function(x) x, simplify = FALSE) |>
    lapply(sf::st_point) |>
    sf::st_as_sfc()

  # all original points expected to lie in cells with NA attribute values
  expect_true(all(is.na(stars::st_extract(d, pts_orig)[["value"]])))

  # apply snapping
  pts_snap <- lapply(pts_orig, \(x) snap_to_nearest_pop_cell(x, d)) |>
    purrr::list_c()

  # all snapped points expected to lie in non-NA cells
  expect_true(all(!is.na(stars::st_extract(d, pts_snap)[["value"]])))

  # test against alternative (less efficient) approach using `sf` functions
  non_na_centres <- sf::st_as_sf(d, as_points = TRUE)
  pts_snap_alt <- non_na_centres$geometry[sf::st_nearest_feature(
    pts_orig,
    non_na_centres
  )]

  expect_identical(pts_snap_alt, pts_snap)

  # Visual evaluation
  pts_linkages <- st_cast(
    st_union(pts_snap, pts_orig, by_feature = TRUE),
    "LINESTRING"
  )

  p <- ggplot() +
    stars::geom_stars(data = d) +
    geom_sf(data = pts_orig, col = "red") +
    geom_sf(data = pts_snap, col = "green") +
    geom_sf(data = pts_linkages, col = "white") +
    theme_minimal() +
    labs(title = "Snapping points to populated cells")

  expect_doppelganger("points snapped to populated cells", p)
})


test_that("snap_to_nearest_pop_cell() returns same point if lying on non-NA cell", {
  m <- matrix(NA_real_, 100, 100)
  # Two populated "islands"
  set.seed(1991)
  m[10:40, 10:40] <- runif(31 * 31, 0, 1)
  m[60:90, 60:90] <- runif(31 * 31, 0, 1)
  d <- stars::st_as_stars(m) |> setNames("value")

  # random sample of 200 points falling in populated cells
  cells_idx <- which(!is.na(d[[1]]), arr.ind = TRUE, useNames = FALSE)
  cells <- cells_idx[sample(nrow(cells_idx), 100), ] - 0.5 # 0.5 take-way to match cell centres

  pts_orig <- cells |>
    apply(1, function(x) x, simplify = FALSE) |>
    lapply(sf::st_point) |>
    sf::st_as_sfc()

  # apply snapping
  pts_snap <- lapply(pts_orig, \(x) snap_to_nearest_pop_cell(x, d)) |>
    purrr::list_c()

  # original and snapped points expected to be equal
  expect_identical(pts_orig, pts_snap)
})


test_that("snap_to_nearest_pop_cell() fails when expected", {
  # random mock point
  p_orig <- st_sfc(st_point(c(49, 49)))

  # `rst` must be a raster (i.e. only 2 dimensions)
  expect_snapshot(
    error = TRUE,
    snap_to_nearest_pop_cell(p_orig, rover_drivers$drv_sst@stars_obj)
  )

  # `rst` must contain only 1 single attribute layer
  expect_snapshot(
    error = TRUE,
    snap_to_nearest_pop_cell(
      p_orig,
      rover_drivers$drv_prey@stars_obj[,,, 1, drop = TRUE] |>
        dplyr::mutate(r = prey^2) # add a 2nd layer
    )
  )

  # `raster`pnt` and `rst` must have equal CRS
  expect_snapshot(
    error = TRUE,
    snap_to_nearest_pop_cell(
      p_orig,
      rover_drivers$drv_prey@stars_obj[,,, 1, drop = TRUE]
    )
  )
})


test_that("dev", {
  # simulate stars object
  m <- matrix(NA_real_, 100, 100)
  # Two populated "islands"
  set.seed(199)
  m[10:40, 10:40] <- runif(31 * 31, 0, 1)
  m[60:90, 60:90] <- runif(31 * 31, 0, 1)
  d <- stars::st_as_stars(m) |> setNames("value")

  # random mock point
  p_orig <- sf::st_sfc(sf::st_point(c(49, 49)))

  bench::mark(
    check = FALSE,
    stars::st_extract(d, p_orig)[[1]],
    snap_to_nearest_pop_cell(p_orig, d)
  )
})


test_that("rebalance_state() dev", {
  skip()

  a = rebalance_states(
    rover_ibm_disnbs@agents[[1]]@condition@states_budget,
    night_prop = 0.5,
    feed_state_id = "foraging",
    roost_state_id = "water_resting",
    curr_energy = units::set_units(-188, "kJ"),
    feed_avg_net_energy = units::set_units(446, "kJ/h"),
    trgt_energy = units::set_units(1, "kJ"),
    step_duration = units::set_units(1, "day")
  )
})

# x <- readRDS("c:/Users/Bruno/Downloads/bioss_spec_map.rds")
# y <- readRDS("c:/Users/Bruno/Downloads/bioss_spec_imp_map.rds")
#
# units::drop_units(sf::st_area(x)[[1]][1])
#
# x |> dplyr::slice(month, 1) |> dplyr::pull(1) |> sum(na.rm = TRUE) # |> plot()
# y |> dplyr::slice(month, 1) |> dplyr::pull(1) |> sum(na.rm = TRUE) # |> plot()
#
# sum(x[[1]]/units::drop_units(sf::st_area(x)[[1]][1]), na.rm = TRUE)
# sum(y[[1]]/units::drop_units(sf::st_area(y)[[1]][1]), na.rm = TRUE)

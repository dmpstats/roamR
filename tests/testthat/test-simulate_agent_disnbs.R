test_that("dev testing", {

  skip("developing puposes only")

  conf <- create_dnbs_config(
    rover_ibm_disnbs@drivers[[1]],
    rover_ibm_disnbs@model_config,
    waypnts_res = 1000,
    ids = list(
      dens_id = "dens",
      intake_id = "intake",
      imp_dens_id = "imp_dens",
      imp_intake_id = "imp_intake",
      feed_state_id = "foraging",
      roost_state_id = "water_resting"
    )
  )

  npr <- derive_night_cube(
    aoc_strs = rover_ibm_disnbs@drivers[[6]] |> stars_obj(),
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
    dnbs_cfg = conf
  )

  plot(history(a)["timestep"], pch = 19)

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
        dnbs_cfg = conf
      )
    }
  )








  set.seed(1000)
  a <- simulate_agent_disnbs_2(
    agent = rover_ibm_disnbs@agents[[1]],
    drivers = rover_ibm_disnbs@drivers,
    states_profile = rover_ibm_disnbs@species@states_profile,
    scen = "baseline",
    night_prop = npr,
    dnbs_cfg = conf
  )

  bench::mark(
    iterations = 25,
    a = {
      set.seed(1234)
      simulate_agent_disnbs_2(
        agent = rover_ibm_disnbs@agents[[1]],
        drivers = rover_ibm_disnbs@drivers,
        states_profile = rover_ibm_disnbs@species@states_profile,
        scen = "baseline",
        night_prop = npr,
        dnbs_cfg = conf
      )
    }
  )











  set.seed(1000)
  a <- simulate_agent_disnbs(
    agent = a,
    dens = rover_ibm_disnbs@drivers[[1]] |> stars_obj(),
    intake = rover_ibm_disnbs@drivers[[2]] |> stars_obj(),
    dnbs_cfg = conf,
    impact = TRUE,
    dens_imp = rover_ibm_disnbs@drivers[[3]] |> stars_obj(),
    intake_imp = rover_ibm_disnbs@drivers[[4]] |> stars_obj()
  )

  plot(history(a)["timestep"], pch = 19)




  rover_ibm_disnbs@species@states_profile$water_rest@energy_cost


  rover_ibm_disnbs@species@states_profile$swimming@energy_cost@fn


  rover_ibm_disnbs@species@states_profile$swimming@energy_cost@fn_cmp(
    agent = rover_ibm_disnbs@agents[[1]],
    drivers = rover_ibm_disnbs@drivers,
    details = TRUE
  )

  rover_ibm_disnbs@species@states_profile$dive@energy_cost@fn_cmp(
    agent = rover_ibm_disnbs@agents[[1]],
    drivers = rover_ibm_disnbs@drivers,
    details = TRUE
  )



  rover_ibm_disnbs@agents[[1]]@properties@cost_par_draws


})




test_that("simulate_agent_disnbs() behaves as expected", {

  withr::local_package("ggplot2")
  withr::local_package("vdiffr")

  # new density driver with no iteration dimension
  d_drv <- Driver(
    id = "tst",
    stars_obj = rover_ibm_disnbs@drivers[[1]] |> stars_obj() |> dplyr::slice(iter, 2)
  )

  imp_d_drv <- Driver(
    id = "tst_imp",
    stars_obj = rover_ibm_disnbs@drivers[[3]] |> stars_obj() |> dplyr::slice(iter, 2)
  )

  d <- d_drv |> stars_obj()
  imp_d <- imp_d_drv |> stars_obj()
  i <- rover_ibm_disnbs@drivers[[2]] |> stars_obj()
  a <- rover_ibm_disnbs@agents[[2]]

  conf <- create_dnbs_config(
    dens_drv = d_drv,
    ibm_cfg = rover_ibm_disnbs@model_config,
    waypnts_res = 100
  )

  #set.seed(19199)
  set.seed(203217)
  sim_a <- simulate_agent_disnbs(a, d, i, conf, impacted = FALSE) |>
    history() |>
    dplyr::mutate(
      timepoint = c(conf$time_grid[1], conf$time_grid),
      months = lubridate::month(timepoint, label = TRUE, abbr = TRUE) |> factor(ordered = FALSE)
    )

  #set.seed(19199)
  set.seed(203217)
  sim_a_imp <- simulate_agent_disnbs(a, d, i, conf, impacted = TRUE, dens_imp = imp_d) |>
    history() |>
    #dplyr::slice(-1) |>
    dplyr::mutate(
      timepoint = c(conf$time_grid[1], conf$time_grid),
      months = lubridate::month(timepoint, label = TRUE, abbr = TRUE) |> factor(ordered = FALSE)
    )

  p <- ggplot() +
    theme_void() +
    stars::geom_stars(data = imp_d) +
    geom_sf(data = sim_a, col = "green", size = 2) +
    geom_sf(data = sim_a_imp, col = "red") +
    scale_fill_viridis_c(transform = "sqrt", option = "B") +
    facet_wrap(vars(months))

  expect_doppelganger("sim_imp_vs_unimp_paths", p)

})





test_that("simulate_agent_disnbs() fails when inputs are invalid", {

  # for brevity
  a <- rover_ibm_disnbs@agents[[1]]
  d <- rover_ibm_disnbs@drivers[[1]]
  i <- rover_ibm_disnbs@drivers[[2]]
  imp_d <-  rover_ibm_disnbs@drivers[[3]]
  imp_i <-  rover_ibm_disnbs@drivers[[4]]
  conf <- create_dnbs_config(d, rover_ibm_disnbs@model_config)

  # wrong `dnbs_cfg`
  expect_snapshot(
    simulate_agent_disnbs(a, d, i, NULL, dnbs_cfg = 1),
    error = TRUE
  )

  # missing impacted drivers when `impact = TRUE`
  expect_snapshot(
    simulate_agent_disnbs(a, d, i, NULL, conf, impacted = TRUE),
    error = TRUE
  )

  expect_snapshot(
    simulate_agent_disnbs(a, d, i, NULL, conf, impacted = TRUE, imp_d),
    error = TRUE
  )

  expect_snapshot(
    simulate_agent_disnbs(a, d, i, NULL, conf, impacted = TRUE, imp_intake_drv = imp_i),
    error = TRUE
  )

})







test_that("calculate_path() works as expected", {

  withr::local_package("ggplot2")
  withr::local_package("vdiffr")

  #calculate_path(agent, dens, crs, impacted = FALSE, dens_imp = NULL)

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
  pth <- calculate_path(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    crs = rover_ibm_disnbs@model_config@ref_sys)

  set.seed(191)
  imp_pth <- calculate_path(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    impacted = TRUE,
    imp_dens = imp_d,
    crs = rover_ibm_disnbs@model_config@ref_sys)

  p <- ggplot() +
    stars::geom_stars(data = imp_d) +
    geom_sf(data = pth, col = "green", linewidth = 1.2) +
    geom_sf(data = imp_pth, col = "red", linewidth = 1.2)

  expect_doppelganger("identical_imp-unimp", p)



  # different endpoints and diverging paths
  set.seed(19199)
  pth <- calculate_path(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    crs = rover_ibm_disnbs@model_config@ref_sys)

  set.seed(19199)
  imp_pth <- calculate_path(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    impacted = TRUE,
    imp_dens = imp_d,
    crs = rover_ibm_disnbs@model_config@ref_sys)

  p <- ggplot() +
    stars::geom_stars(data = imp_d) +
    geom_sf(data = pth, col = "green", linewidth = 1.2) +
    geom_sf(data = imp_pth, col = "red", linewidth = 1.2)

  expect_doppelganger("diff_endpoints", p)


  # same end point but impacted path diverge to avoid hole
  set.seed(23)
  pth <- calculate_path(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    crs = rover_ibm_disnbs@model_config@ref_sys)

  set.seed(23)
  imp_pth <- calculate_path(
    rover_ibm_disnbs@agents[[1]],
    dens = d,
    impacted = TRUE,
    imp_dens = imp_d,
    crs = rover_ibm_disnbs@model_config@ref_sys)


  p <- ggplot() +
    stars::geom_stars(data = imp_d) +
    geom_sf(data = pth, col = "green", linewidth = 1.2) +
    geom_sf(data = imp_pth, col = "red", linewidth = 1.2)

  expect_doppelganger("same_endpoint_diff_path", p)



  # # ------
  #
  # a <- rover_ibm_disnbs@agents[[1]]
  # location(a) <- location(a) + 1
  #
  # set.seed(23)
  # pth <- calculate_path(
  #   a,
  #   dens = d,
  #   crs = rover_ibm_disnbs@model_config@ref_sys)
  #
  # set.seed(23)
  # imp_pth <- calculate_path(
  #   rover_ibm_disnbs@agents[[1]],
  #   dens = d,
  #   impacted = TRUE,
  #   dens_imp = imp_d,
  #   crs = rover_ibm_disnbs@model_config@ref_sys)
  #
  #
  # p <- ggplot() +
  #   stars::geom_stars(data = imp_d) +
  #   geom_sf(data = pth, col = "green", linewidth = 1.2) +
  #   geom_sf(data = imp_pth, col = "red", linewidth = 1.2)


})






test_that("extract_dns_layer() works as expected", {

  # dens with temp and iter dimensions
  i <- 1

  conf <- create_dnbs_config(
    rover_ibm_disnbs@drivers[[1]],
    rover_ibm_disnbs@model_config
  )

  set.seed(10)
  out <- extract_dns_layer(
    timestep = conf$routing_timesteps[i],
    dns_strs = rover_ibm_disnbs@drivers[[1]] |> stars_obj(),
    conf
  )

  set.seed(10)
  expected <- rover_ibm_disnbs@drivers[[1]] |>
    stars_obj() |>
    dplyr::slice(months, conf$dns_tm_slices[i]) |>
    dplyr::slice(iter, sample(conf$dns_itr_slices, 1))

  expect_equal(out, expected)
  expect_length(dim(out), 2)


  # dens with only temporal dimension
  d <- Driver("t", stars_obj = rover_ibm_disnbs@drivers[[1]] |> stars_obj() |> dplyr::slice(iter, 2))
  conf <- create_dnbs_config(d, rover_ibm_disnbs@model_config)

  i <- 2
  out <- extract_dns_layer(timestep = conf$routing_timesteps[i], dns_strs = stars_obj(d), conf)
  expected <- stars_obj(d) |> dplyr::slice(months, conf$dns_tm_slices[i])

  expect_equal(out, expected)
  expect_length(dim(out), 2)


  # dens with only iteration dimension
  d <- Driver("t", stars_obj = rover_ibm_disnbs@drivers[[1]] |> stars_obj() |> dplyr::slice(months, 2))
  conf <- create_dnbs_config(d, rover_ibm_disnbs@model_config)

  set.seed(199)
  out <- extract_dns_layer(timestep = conf$routing_timesteps[i], dns_strs = stars_obj(d), conf)
  set.seed(199)
  expected <- stars_obj(d) |> dplyr::slice(iter, sample(conf$dns_itr_slices, 1))

  expect_equal(out, expected)
  expect_length(dim(out), 2)



  # raster-only density (i.e no tepporal nor iteration dimensions)
  d <- Driver("t", stars_obj = generate_mock_stars(attr_units = "g"))
  conf <- create_dnbs_config(d, rover_ibm_disnbs@model_config)

  out <- extract_dns_layer(timestep = conf$routing_timesteps[i], dns_strs = stars_obj(d), conf)
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
    } ,
    "All values in the first attribute of `x` must be non-negative."
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
    feed_avg_net_energy =  units::set_units(446, "kJ/h"),
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


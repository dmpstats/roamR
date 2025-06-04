test_that("dev testing", {

  skip("developing puposes only")

  # initialize ibm for mock species
  x <- rmr_initiate(ibm_config_rover, rover, rover_drivers)
  rover_ibm_disnbs@species@driver_responses


  run_disnbs(
    ibm = x,
    dens_id = "rvr_distr",
    intake_id = "prey_distr",
    impact = FALSE
  )


  #future::plan(future::sequential())
  future::plan(future::multisession, workers = future::availableCores() - 10)

  res <- run_disnbs(
    ibm = rover_ibm_disnbs,
    dens_id = "dens",
    intake_id = "intake",
    feed_state_id = "foraging",
    roost_state_id = "water_resting",
    run_scen = "baseline-and-impact",
    feed_avg_net_energy = units::set_units(422, "kJ/h"),
    target_energy = units::set_units(1, "kJ"),
    imp_dens_id = "imp_dens",
    imp_intake_id = "imp_intake",
    smooth_body_mass = bm_smooth_opts(time_bw = "7 days")
    #seed = 109
  )

  future::plan(future::sequential())

})


# run_disnbs() Negative Testing ----------------------

test_that("run_disnbs() fails when driver IDs required under specified `scen` are not specified", {

  # missing `intake_id`
  expect_snapshot(
    run_disnbs(rover_ibm_disnbs, run_scen = "baseline",
               dens_id = "dens",
               feed_state_id = "f", roost_state_id = "ro", quiet = TRUE),
    error = TRUE
  )

  expect_snapshot(
    run_disnbs(rover_ibm_disnbs, run_scen = "baseline-and-impact",
               dens_id = "dens",
               feed_state_id = "f", roost_state_id = "ro", quiet = TRUE),
    error = TRUE
  )

  # missing `imp_dens_id`
  expect_snapshot(
    run_disnbs(rover_ibm_disnbs, run_scen = "impact",
               dens_id = "dens", intake_id = "intake",
               feed_state_id = "f", roost_state_id = "ro", quiet = TRUE),
    error = TRUE
  )

  expect_snapshot(
    run_disnbs(rover_ibm_disnbs, run_scen = "baseline-and-impact",
               dens_id = "dens", intake_id = "intake",
               feed_state_id = "f", roost_state_id = "ro", quiet = TRUE
    ),
    error = TRUE
  )

  # missing `imp_intake_id`
  expect_snapshot(
    run_disnbs(rover_ibm_disnbs, run_scen = "impact", dens_id = "dens",
               intake_id = "intake", imp_dens_id = "imp_dens",
               feed_state_id = "f", roost_state_id = "ro", quiet = TRUE),
    error = TRUE
  )

})


test_that("run_disnbs() fails when baseline and impacted density maps have inconsistent dimensions", {

  # different number of iterations
  x <- rover_ibm_disnbs
  x@drivers[[3]] <- Driver(
    "imp_dens",
    stars_obj = rover_ibm_disnbs@drivers[[3]] |> stars_obj() |> dplyr::slice(iter, 2:3)
  )

  expect_snapshot(
    run_disnbs(x, run_scen = "baseline-and-impact", dens_id = "dens", intake_id = "intake",
               imp_intake_id = "imp_intake", imp_dens_id = "imp_dens",
               feed_state_id = "f", roost_state_id = "ro", quiet = TRUE),
    error = TRUE
  )

  # different temporal dimensions
  x@drivers[[3]] <- Driver(
    "imp_dens",
    stars_obj = rover_ibm_disnbs@drivers[[3]] |> stars_obj() |> dplyr::slice(months, 2:3)
  )

  expect_snapshot(
    run_disnbs(x, run_scen = "baseline-and-impact", dens_id = "dens",
               intake_id = "intake", imp_intake_id = "imp_intake",
               imp_dens_id = "imp_dens",
               feed_state_id = "f", roost_state_id = "ro", quiet = TRUE),
    error = TRUE
  )
})



test_that("run_disnbs() fails when inputs are not specified in expected contextual units", {

  # wrong units for `waypnts_res`
  expect_snapshot(
    run_disnbs(ibm = x, dens_id = "dens", intake_id = "intake",
               feed_state_id = "f", roost_state_id = "ro",
               waypnts_res = units::set_units(2, "g"), quiet = TRUE),
    error = TRUE
  )

  # feed_avg_net_energy
  expect_snapshot(
    run_disnbs(ibm = x, dens_id = "dens", intake_id = "intake",
               feed_state_id = "f", roost_state_id = "ro",
               feed_avg_net_energy = units::set_units(2, "km/h"), quiet = TRUE),
    error = TRUE
  )

  # target_energy
  expect_snapshot(
    run_disnbs(ibm = x, dens_id = "dens", intake_id = "intake",
               feed_state_id = "f", roost_state_id = "ro",
               target_energy = units::set_units(2, "J/h"), quiet = TRUE),
    error = TRUE
  )


  # intake maps
  x <- rover_ibm_disnbs
  x@drivers[[7]] <- Driver(
    id = "bla",
    stars_obj = stars_obj(pluck_s4(x@drivers, "intake"))
    )

  stars_obj(x@drivers[[7]])[[1]]  <- stars_obj(x@drivers[[7]])[[1]] |>
    units::drop_units() |>
    units::set_units("kJ")

  expect_snapshot(
    run_disnbs(ibm = x, dens_id = "dens", intake_id = "bla",
               feed_state_id = "f", roost_state_id = "ro", quiet = TRUE),
    error = TRUE
  )

})





test_that("run_disnbs() fails when expected: misc", {

  # specified drivers IDs are not present in IBM
  expect_snapshot(
    run_disnbs(ibm = rover_ibm_disnbs, dens_id = "NON-EXISTENT_DRIVER", intake_id = "intake",
               feed_state_id = "foraging", roost_state_id = "water_resting", quiet = TRUE),
    error = TRUE
  )

  expect_snapshot(
    run_disnbs(ibm = rover_ibm_disnbs, run_scen = "baseline-and-impact",
               dens_id = "dens", intake_id = "intake",
               imp_dens_id = "GONE_DRIVER", imp_intake_id = "ABSENT-DRIVER",
               feed_state_id = "foraging", roost_state_id = "water_resting", quiet = TRUE),
    error = TRUE
  )


  # <stars> objects of specified drivers IDs are empty
  x <- rover_ibm_disnbs
  x@drivers[[1]] <- Driver(id = "dens")

  expect_snapshot(
    run_disnbs(ibm = x, run_scen = "baseline",
               dens_id = "dens", intake_id = "intake",
               imp_dens_id = "GONE_DRIVER", imp_intake_id = "ABSENT-DRIVER",
               feed_state_id = "foraging", roost_state_id = "water_resting", quiet = TRUE),
    error = TRUE
  )

  # <stars> objects of specified drivers IDs must have one unique attribute
  x <- rover_ibm_disnbs
  x@drivers[[1]]@stars_obj$extra_dim <- x@drivers[[1]]@stars_obj$counts * 10

  expect_snapshot(
    run_disnbs(ibm = x, run_scen = "baseline",
               dens_id = "dens", intake_id = "intake",
               imp_dens_id = "GONE_DRIVER", imp_intake_id = "ABSENT-DRIVER",
               feed_state_id = "foraging", roost_state_id = "water_resting", quiet = TRUE),
    error = TRUE
  )


  # density surface doesn't cover all time steps of intended model
  x <- rover_ibm_disnbs

  x@drivers[[1]] <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_units = "g",
      time_name = "month",
      time_type = "numeric")
  )

  expect_snapshot(
    run_disnbs(ibm = x, dens_id = "d", intake_id = "intake",
               feed_state_id = "foraging", roost_state_id = "water_resting", quiet = TRUE),
    error = TRUE
  )

  # Inconsistent CRSs between surfaces and model_config specification
  x <- rover_ibm_disnbs
  x@drivers[[1]]@stars_obj <- sf::st_set_crs(x@drivers[[1]]@stars_obj, 5556)

  expect_snapshot(
    run_disnbs(x, run_scen = "baseline-and-impact", dens_id = "dens",
               intake_id = "intake", imp_intake_id = "imp_intake",
               imp_dens_id = "imp_dens",
               feed_state_id = "f", roost_state_id = "ro", quiet = TRUE),
    error = TRUE
  )


  # feed_state_id and roost_state_id: non-existent state ids
  expect_snapshot(
    run_disnbs(rover_ibm_disnbs, run_scen = "baseline",
               dens_id = "dens", intake_id = "intake",
               feed_state_id = "f", roost_state_id = "ro", quiet = TRUE),
    error = TRUE
  )

  expect_snapshot(
    run_disnbs(rover_ibm_disnbs, run_scen = "baseline",
               dens_id = "dens", intake_id = "intake",
               feed_state_id = "foraging", roost_state_id = "ro", quiet = TRUE),
    error = TRUE
  )


})






test_that("create_dnbs_config() works as expected", {

  drv_ids = list(
    dens_id = "dens",
    intake_id = "intake",
    imp_dens_id = "imp_dens",
    imp_intake_id = "imp_intake",
    feed_state_id = "foraging",
    roost_state_id = "water_resting"
  )

  d <- rover_ibm_disnbs@drivers[[1]]
  md_cfg <- rover_ibm_disnbs@model_config

  expect_s3_class(create_dnbs_config(d, md_cfg, ids = drv_ids), "disnbs_config")


  # raster-only density driver
  d <- Driver(id = "d", stars_obj = generate_mock_stars(attr_units = "g"))
  out <- create_dnbs_config(d, md_cfg,  ids = drv_ids)

  expect_true(is.na(out$dns_tm_dim))
  expect_null(out$dns_tm_slices)
  expect_true(is.na(out$dns_itr_dim))
  expect_equal(out$routing_timesteps, 1)


  # density with iterations only
  d <- Driver("d", stars_obj = generate_mock_stars(attr_units = "g", iter_name =  "boot"))
  out <- create_dnbs_config(d, md_cfg,  ids = drv_ids)

  expect_true(is.na(out$dns_tm_dim))
  expect_null(out$dns_tm_slices)
  expect_equal(out$dns_itr_dim, 3)
  expect_equal(out$routing_timesteps, 1)


  # density with both temporal and iteration dimensions
  out <- create_dnbs_config(rover_ibm_disnbs@drivers[[1]], rover_ibm_disnbs@model_config, id = drv_ids)

  expect_equal(out$dns_tm_dim, 3)
  expect_equal(out$dns_itr_dim, 4)
  expect_length(out$routing_timesteps, dim(rover_ibm_disnbs@drivers[[1]]@stars_obj)[3])

  expect_equal(out$routing_timesteps, c(1, 32, 62, 93, 124, 152))
  expect_equal(out$dns_tm_slices, c(4, 5, 6, 1, 2, 3))


})






test_that("derive_night_cube() works as expected", {

  withr::local_package("vdiffr")

  aoc_drv <- purrr::keep(rover_ibm_disnbs@drivers, \(d) d@id == "aoc")[[1]]

  # AOC CRS in latlon
  night_prop <- derive_night_cube(
    aoc_strs = stars_obj(aoc_drv),
    start_date = rover_ibm_disnbs@model_config@start_date,
    end_date = rover_ibm_disnbs@model_config@end_date,
    delta_time = "1 week"
  )

  expect_s3_class(night_prop, "stars")
  expect_length(dim(night_prop), 3)
  expect_true(all(night_prop[[1]] >= 0))
  expect_true(all(night_prop[[1]] <= 1))
  expect_true(sf::st_crs(night_prop) == sf::st_crs(stars_obj(aoc_drv)))

  # plot snapshot - detecting changes
  expect_doppelganger(
    "night_cube_latlon",
    plot(night_prop, col = viridisLite::mako(50, direction = -1))
  )

  # AOC CRS in UTM
  stars_obj(aoc_drv) <- stars::st_warp(aoc_drv@stars_obj, crs = sf::st_crs(32629))

  night_prop <- derive_night_cube(
    aoc_strs = stars_obj(aoc_drv),
    start_date = rover_ibm_disnbs@model_config@start_date,
    end_date = rover_ibm_disnbs@model_config@end_date,
    #crs = rover_ibm_disnbs@model_config@ref_sys,
    delta_time = "1 week"
  )

  # Input and output have the same CRS
  expect_true(sf::st_crs(night_prop) == sf::st_crs(stars_obj(aoc_drv)))

  # plot snapshot
  expect_doppelganger(
    "night_cube_utm",
    plot(night_prop, col = viridisLite::mako(50, direction = -1))
  )


})





test_that("create_dnbs_config() dev testing", {

  skip()

  d <- Driver(id = "d", stars_obj = generate_mock_stars(attr_units = "g"))
  create_dnbs_config(d, rover_ibm_disnbs@model_config, ids = list(dens = "ja"))

  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(attr_units = "g", iter_name =  "boot")
  )

  create_dnbs_config(rover_ibm_disnbs@drivers[[1]], rover_ibm_disnbs@model_config)

})

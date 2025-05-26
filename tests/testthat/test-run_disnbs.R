test_that("dev testing", {

  skip("developing puposes only")

  # initialize ibm for mock species
  x <- rmr_initiate(ibm_config_rover, rover, rover_drivers)


  class(rover_ibm_disnbs)
  class(x)

  rover_ibm_disnbs@species@driver_responses


  run_disnbs(
    ibm = x,
    dens_id = "rvr_distr",
    intake_id = "prey_distr",
    impact = FALSE
  )


  run_disnbs(
    ibm = rover_ibm_disnbs,
    dens_id = "dens",
    intake_id = "intake",
    impact = FALSE
  )




  run_disnbs(
    ibm = rover_ibm_disnbs,
    dens_id = "dens",
    intake_id = "intake",
    impact = TRUE,
    imp_dens_id = "imp_dens",
    imp_intake_id = "imp_intake"
  )

})





test_that("run_disnbs() fails when expected", {

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
    run_disnbs(ibm = x, dens_id = "d", intake_id = "intake"),
    error = TRUE
  )

  # wrong units for `waypnts_res`
  expect_snapshot(
    run_disnbs(ibm = x, dens_id = "d", intake_id = "intake", waypnts_res = units::set_units(2, "g")),
    error = TRUE
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




test_that("create_dnbs_config() works as expected", {

  # raster-only density driver
  d <- Driver(id = "d", stars_obj = generate_mock_stars(attr_units = "g"))
  out <- create_dnbs_config(d, rover_ibm_disnbs@model_config)

  expect_true(is.na(out$dns_tm_dim))
  expect_null(out$dns_tm_slices)
  expect_true(is.na(out$dns_itr_dim))
  expect_equal(out$routing_timesteps, 1)

  # density with iterations
  d <- Driver("d", stars_obj = generate_mock_stars(attr_units = "g", iter_name =  "boot"))
  out <- create_dnbs_config(d, rover_ibm_disnbs@model_config)

  expect_true(is.na(out$dns_tm_dim))
  expect_null(out$dns_tm_slices)
  expect_equal(out$dns_itr_dim, 3)
  expect_equal(out$routing_timesteps, 1)


  # density with both temporal and iteration dimensions
  out <- create_dnbs_config(rover_ibm_disnbs@drivers[[1]], rover_ibm_disnbs@model_config)

  expect_equal(out$dns_tm_dim, 3)
  expect_equal(out$dns_itr_dim, 4)
  expect_length(out$routing_timesteps, dim(rover_ibm_disnbs@drivers[[1]]@stars_obj)[3])
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


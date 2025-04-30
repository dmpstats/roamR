test_that("errors raised when discrepancies between drivers and driver-related cost functions are found", {

  # all good
  expect_null(init_check_consistency(rover, rover_drivers, ibm_config_rover))

  # incompatible units
  r <- rover
  r@states_profile$swimming@energy_cost@args_spec$sst@units <- "m"

  expect_error(
    init_check_consistency(r, rover_drivers),
    class = "err-noncovertible-units"
  )

  # Missing driver ID in `drivers`
  expect_error(
    init_check_consistency(
      rover,
      rover_drivers |> purrr::discard(~.@id == "sst")
    ),
    class = "err-nonexistent-driverid"
  )

  # Unavailable raster data
  d <- rover_drivers
  stars_obj(d$drv_sst) <- stars::st_as_stars(matrix(NA))

  expect_error(
    init_check_consistency(rover, d),
    class =  "err-nonexistent-raster-in-driver"
  )

})




test_that("top-level input errors are detected", {

  # multiple driver IDs
  d <- rover_drivers
  d$drv_land@id <- "sst"

  expect_error(
    init_check_consistency(rover, d),
    class =  "err-multiple-driverid"
  )

  d$drv_sss@id <- "prey_distr"
  expect_error(
    init_check_consistency(rover, d),
    class =  "err-multiple-driverid"
  )


  # non-existent drivers specified in species@driver_responses
  r <- rover
  r@driver_responses[[1]]@driver_id <- "poo"

  expect_error(
    init_check_consistency(r, rover_drivers),
    class = "err-nonexistent-driverid"
  )


  # CRS mismatch between drivers and expected by model_config
  m <- ibm_config_rover
  m@ref_sys <- sf::st_crs(32604)

  expect_error(
    init_check_consistency(rover, rover_drivers, m),
    class = "err-crs-mismatch"
  )


  # spatial inconsistencies between drivers and AOC
  #library(ggplot2)
  # ggplot() +
  #   geom_sf(data = sf::st_as_sfc(m@aoc_bbx),  col = "red", fill = "red", alpha = 0.1) +
  #   ylim(c(54, 61)) +
  #   xlim(c(-5, 6)) +
  #   geom_sf(data = rover_drivers$drv_trawling@sf_obj) +
  #   geom_sf(data = rover_drivers$drv_land@sf_obj) +
  #   geom_sf(data = rover_drivers$drv_owfs@sf_obj, fill = "blue", alpha = 0.2)

  m <- ibm_config_rover
  m@aoc_bbx <- structure(
    c(-4.5, 59.5, 1, 61),
    names = c("xmin", "ymin", "xmax", "ymax"),
    class = "bbox",
    crs = sf::st_crs(4326))

  # sf objects
  d <- rover_drivers[c("drv_land", "drv_trawling")]
  expect_error(
    init_check_consistency(Species(), d, m),
    class = "err-driver-outside-aoc"
    )

  d <- rover_drivers[c("drv_land", "drv_owfs")]
  expect_warning(
    init_check_consistency(Species(), d, m),
    "4/5 of geometric features in driver ID",
    class = "wrn-driver-partial-aoc"
  )

  # star objects
  d <- rover_drivers[c("drv_sst")]
  m@aoc_bbx <- structure(
    c(-10, 55.5, -5, 61),
    names = c("xmin", "ymin", "xmax", "ymax"),
    class = "bbox",
    crs = sf::st_crs(4326))

  # ggplot() +
  #   geom_sf(data = sf::st_as_sfc(m@aoc_bbx),  col = "red", fill = "red", alpha = 0.1) +
  #   ylim(c(54, 62)) +
  #   xlim(c(-10.5, 4)) +
  #   stars::geom_stars(data = d$drv_sst@stars_obj[, , , "January"])

  expect_error(
    init_check_consistency(Species(), d, m),
    class = "err-driver-outside-aoc"
  )


  m@aoc_bbx <- structure(
    c(-10, 60, -4.5, 61),
    names = c("xmin", "ymin", "xmax", "ymax"),
    class = "bbox",
    crs = sf::st_crs(4326))

  # ggplot() +
  #   geom_sf(data = sf::st_as_sfc(m@aoc_bbx),  col = "red", fill = "red", alpha = 0.1) +
  #   ylim(c(54, 62)) +
  #   xlim(c(-10.5, 4)) +
  #   stars::geom_stars(data = d$drv_sst@stars_obj[, , , "January"])

  expect_warning(
    init_check_consistency(Species(), d, m),
    class = "wrn-driver-partial-aoc"
  )

})

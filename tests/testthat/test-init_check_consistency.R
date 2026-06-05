# Activity States -------
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
      rover_drivers |> purrr::discard(~ .@id == "sst")
    ),
    class = "err-nonexistent-driverid"
  )

  # Unavailable raster data
  d <- rover_drivers
  d$drv_sst@stars_obj <- stars::st_as_stars(matrix(NA))

  expect_error(
    init_check_consistency(rover, d),
    class = "err-nonexistent-raster-in-driver"
  )
})


test_that("errors raised when required activity types are missing in states profile under density-informed movement", {
  cfg <- ibm_config_rover
  cfg@movement_model <- "di"

  # all good
  expect_null(init_check_consistency(rover, rover_drivers, cfg))

  # missing states with with required activity types ("resting" and "flying")
  r <- rover
  r@states_profile$water_rest@type <- "other"

  expect_snapshot(
    init_check_consistency(r, rover_drivers, cfg),
    error = TRUE,
    cnd_class = TRUE
  )

  r@states_profile$dive@type <- "other"
  expect_snapshot(
    init_check_consistency(r, rover_drivers, cfg),
    error = TRUE,
    cnd_class = TRUE
  )

  # duplicated required states with required activity type
  r <- rover
  r@states_profile$swimming@type <- "foraging"

  expect_snapshot(
    init_check_consistency(r, rover_drivers, cfg),
    error = TRUE,
    cnd_class = TRUE
  )

  r@states_profile$flight@type <- "resting"

  expect_snapshot(
    init_check_consistency(r, rover_drivers, cfg),
    error = TRUE,
    cnd_class = TRUE
  )
})


test_that("top-level input errors are detected", {
  # multiple driver IDs
  d <- rover_drivers
  d$drv_land@id <- "sst"

  expect_snapshot(
    init_check_consistency(rover, d),
    error = TRUE,
    cnd_class = TRUE
  )

  d$drv_sss@id <- "prey_distr"
  expect_snapshot(
    init_check_consistency(rover, d),
    error = TRUE,
    cnd_class = TRUE
  )

  # non-existent drivers specified in species@driver_responses
  r <- rover
  r@driver_responses[[1]]@driver_id <- "poo"

  expect_snapshot(
    init_check_consistency(r, rover_drivers),
    error = TRUE,
    cnd_class = TRUE
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
    crs = sf::st_crs(4326)
  )

  # sf objects
  d <- rover_drivers[c("drv_land", "drv_trawling")]
  expect_snapshot(
    init_check_consistency(Species(), d, m),
    error = TRUE,
    cnd_class = TRUE
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
    crs = sf::st_crs(4326)
  )

  # ggplot() +
  #   geom_sf(data = sf::st_as_sfc(m@aoc_bbx),  col = "red", fill = "red", alpha = 0.1) +
  #   ylim(c(54, 62)) +
  #   xlim(c(-10.5, 4)) +
  #   stars::geom_stars(data = d$drv_sst@stars_obj[, , , "January"])

  expect_snapshot(
    init_check_consistency(Species(), d, m),
    error = TRUE,
    cnd_class = TRUE
  )

  m@aoc_bbx <- structure(
    c(-10, 60, -4.5, 61),
    names = c("xmin", "ymin", "xmax", "ymax"),
    class = "bbox",
    crs = sf::st_crs(4326)
  )

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


# `ModelConfig` start/end_sites Vs Density maps under density-informed movement ------------------------

test_that("density-informed movement: init halts when start/end sites inconsistent with dens maps", {
  skip()

  m <- ibm_config_rover
  movement_model(m) <- "di"
  d <- rover_drivers
  s <- rover
  s@driver_responses[[3]]@movement@mode <- ""

  # returns NULL if now issue found
  expect_null(init_check_consistency(s, d, m))

  # errors when START POINTS outside density map
  m@start_sites <- start_sites(m) |>
    dplyr::mutate(geom = geom + c(0, 10, 10)) |>
    sf::st_set_crs(sf::st_crs(m@ref_sys))

  expect_snapshot(
    init_check_consistency(s, d, m),
    error = TRUE
  )

  # errors when END POINTS outside density map
  m <- ibm_config_rover
  movement_model(m) <- "di"

  m@end_sites <- start_sites(m) |>
    dplyr::mutate(geom = geom + c(0, 0, 10)) |>
    sf::st_set_crs(sf::st_crs(m@ref_sys))

  expect_snapshot(
    error = TRUE,
    init_check_consistency(s, d, m)
  )

  # errors when START POLYGON outside density map
  m <- ibm_config_rover
  movement_model(m) <- "di"
  m@start_sites <- sf::st_buffer(start_sites(m), 170000)

  expect_snapshot(
    error = TRUE,
    init_check_consistency(s, d, m)
  )

  # check for no errors after cropping site polygons to driver's boundaries
  drv_poly <- st_as_sfc(d$drv_sp_distr@stars_obj, as_points = FALSE) |>
    st_union()

  suppressWarnings(
    m@start_sites <- st_crop(
      m@start_sites,
      # shrinking to ensure cropped geoms completely inside driver borders
      sf::st_buffer(drv_poly, dist = units::set_units(-0.005, "degrees"))
    )
  )

  expect_null(init_check_consistency(s, d, m))

  # st_within(m@start_sites, drv_poly, sparse = FALSE)
  # st_covered_by(m@start_sites, drv_poly, sparse = FALSE)
  # st_contains(drv_poly, m@start_sites, sparse = FALSE)
  #
  # ggplot2::ggplot() +
  #   stars::geom_stars(data = rover_drivers$drv_sp_distr@stars_obj) +
  #   ggplot2::geom_sf(data = m@start_sites)
})

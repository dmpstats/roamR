test_that("rmr_initiate() creates a <IBM> S4 object", {
  out <- rmr_initiate(ModelConfig(), Species(), Driver(), quiet = TRUE)
  expect_s4_class(out, class = "IBM")
  expect_s4_class(out@agents[[1]], class = "Agent")

})




test_that("top-level functionality works as expected", {

  # expected number of agents
  m <- ibm_config_rover
  m@n_agents <- 50L
  out <- rmr_initiate(m, rover, rover_drivers, quiet = TRUE)

  # outputs expected number of agents
  expect_length(out@agents, m@n_agents)

  # inconsistency between spatial objects in detected: in this case AOC vs land
  # Note: more thorough testing performed in "test-init_check_consistency.r"
  expect_error(
    rmr_initiate(ModelConfig(), rover, rover_drivers, quiet = TRUE)
  )

  # All generated agents are located inside the specified AOC
  aoc_poly <- sf::st_as_sfc(out@model_config@aoc_bbx)

  agent_locs <- lapply(out@agents, location) |>
    sf::st_as_sfc() |>
    sf::st_set_crs(out@model_config@ref_sys)

  expect_true(all(sf::st_contains(aoc_poly, agent_locs, sparse = FALSE)))

  # AOC driver is generated
  expect_false(
    purrr::detect(out@drivers, \(d) d@id == "aoc") |>
      stars_obj() |>
      is_stars_empty()
  )

  # Species movement-based response to AOC driver is generated
  expect_false(
    purrr::detect(out@species@driver_responses, \(d) d@driver_id == "aoc") |>
      slot("movement") |>
      slot("fn") |>
      rlang::is_empty()
  )

})



test_that("drivers are correctly cropped to AOC", {

  library(ggplot2)
  library(vdiffr)

  # modify example model config for testing
  m <- ibm_config_rover
  m@n_agents <- 1L

  m@aoc_bbx <- structure(
    c(-4, 55.8, 2.5, 56.8),
    names = c("xmin", "ymin", "xmax", "ymax"),
    class = "bbox",
    crs = sf::st_crs(4326))

  # select land and sst only
  d <- rover_drivers[c("drv_land", "drv_sst")]

  out <- rmr_initiate(m, Species(), d, quiet = TRUE)

  # land (sf) and SST (stars)
  p <- ggplot() +
    stars::geom_stars(data = out@drivers$drv_sst@stars_obj[, , , "January"]) +
    geom_sf(data = out@drivers$drv_land@sf_obj) +
    geom_sf(data = sf::st_as_sfc(out@model_config@aoc_bbx), linewidth = 1, col = "red", fill = NA, alpha = 0.1)

  # graphical check via svg-based snapshot test
  expect_doppelganger("land cropped to aoc", p)

})




test_that("CRS-differing drivers are transformed to match ModelConfig@ref_sys", {

  # modify example model config for testing
  m <- ibm_config_rover
  m@n_agents <- 1L

  # set bbox to NAD83 CRS
  m@ref_sys <- sf::st_crs(4269)
  m@aoc_bbx <- structure(
    c(-4, 55.8, 2.5, 56.8),
    names = c("xmin", "ymin", "xmax", "ymax"),
    class = "bbox",
    crs = sf::st_crs(4269))

  # select land and sst only
  d <- rover_drivers[c("drv_land", "drv_sst")]


  expect_no_error(
    out <- rmr_initiate(m, Species(), d, quiet = TRUE)
  )

  expect_identical(
    sf::st_crs(out@drivers$drv_sst@stars_obj),
    out@model_config@ref_sys
  )

  expect_identical(
    sf::st_crs(out@drivers$drv_land@sf_obj),
    out@model_config@ref_sys
  )

})






test_that("Progress messages rendered as expected", {

  withr::local_options(cli.dynamic = TRUE, cli.ansi = TRUE)

  # modify example model config for testing
  m_crw <- ibm_config_rover
  m_crw@n_agents <- 2L

  # CRW movement | Full spatial consistency
  msg <- capture_cli_messages(
    rmr_initiate(m_crw, rover, rover_drivers)
  ) |>
    fix_times()


  # density-informed movement
  m_di <- m_crw
  m_di@movement_type <- "di"

  msg <- capture_cli_messages(
    rmr_initiate(m_di, rover, rover_drivers)
  ) |>
    fix_times()

  expect_snapshot(msg)

  # Driver CRS conversion
  m_di@ref_sys <- sf::st_crs(4269)
  m_di@aoc_bbx <- structure(
    c(-4, 55.8, 2.5, 56.8),
    names = c("xmin", "ymin", "xmax", "ymax"),
    class = "bbox",
    crs = sf::st_crs(4269))
  d <- rover_drivers[c("drv_land", "drv_sst")]

  msg <- capture_cli_messages(
    rmr_initiate(m_di, Species(), d)
  ) |>
    fix_times()

  expect_snapshot(msg)

  # Curvilinear Driver converted to egular grid
  stars_obj(d$drv_sst) <- sf::st_transform(d$drv_sst@stars_obj, sf::st_crs(d$drv_sst@stars_obj))

  msg <- capture_cli_messages(
    suppressWarnings(rmr_initiate(m_crw, Species(), d))
  ) |>
    fix_times()

  expect_snapshot(msg)
})









test_that("development testing", {

  skip()

  m <- ibm_config_rover
  m@n_agents <- 5L

  out <- rmr_initiate(ibm_config_rover, rover, rover_drivers)

})

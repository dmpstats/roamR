test_that("Validation of site slots work as expected", {

  pts = matrix(1:8, ncol = 2)
  npts <- nrow(pts)
  sites <- sf::st_sf(id = letters[1:npts], prop = 1/npts, geom = apply(pts, 1, sf::st_point, simplify = FALSE), crs = 4326)

  expect_error(
    ModelConfig(
      aoc_bbx = c(0, 0, -10, -10),
      start_sites = sites
    ),
    "sites are located outside the AOC area"
  )

  expect_error(
    ModelConfig(start_sites = sites |> sf::st_transform(3441)),
    "must have the same CRS"
  )

  expect_error(
    ModelConfig(end_sites = sites |> dplyr::select(-id)),
    "must contain column"
  )

  expect_error(
    ModelConfig(end_sites = sites |> dplyr::select(-prop)),
    "must contain column"
  )

  expect_error(
    ModelConfig(end_sites = sites |> dplyr::mutate(prop = 0.1)),
    "must add up to 1"
  )

  expect_error(
    ModelConfig(
      start_sites = sites |> sf::st_transform(3441),
      end_sites = sites |> dplyr::mutate(prop = 0.1)
    ),
    "must have the same CRS as @aoc_bbx"
  )

  # test validation via validObject() on modified slot
  x <- ModelConfig()
  x@start_sites <-  sites |> sf::st_transform(3441)
  expect_error(validObject(x), "must have the same CRS as @aoc_bbx")

  x <- ModelConfig()
  x@movement_type <- "INVALID-MOVEMENT"
  expect_error(validObject(x), 'slot @movement_type: Invalid value')

})




test_that("Missing values for required slots raises errors", {

  # n_agents
  expect_snapshot(
    ModelConfig(n_agents = NA_integer_),
    error = TRUE
  )

  # CRS
  expect_snapshot(
    ModelConfig(ref_sys = sf::st_crs(NA)),
    error = TRUE
  )

  # AOC bbox
  expect_snapshot(
    ModelConfig(aoc_bbx = c(1, 2, NA, NA)),
    error = TRUE
  )

  # Spatial resolution for CRW movement models
  expect_snapshot(
    ModelConfig(
      movement_type = "crw",
      delta_x = NA_real_, delta_y = NA_real_
    ),
    error = TRUE
  )

  # Simulation period
  expect_snapshot(
    ModelConfig(start_date = as.Date(NA), end_date = as.Date(NA)),
    error = TRUE
  )

  # Temporal resolution
  expect_snapshot(
    ModelConfig(delta_time = NA_character_),
    error = TRUE
  )

})



test_that("Error raised when units of @delta_time are invalid", {

  expect_snapshot(
    ModelConfig(delta_time = "WRONG_UNITS"),
    error = TRUE
  )

  expect_no_error(ModelConfig(delta_time = "1 day"))
  expect_no_error(ModelConfig(delta_time = "10 week"))
  expect_no_error(ModelConfig(delta_time = "1 month"))
  expect_no_error(ModelConfig(delta_time = "3 months"))
  expect_no_error(ModelConfig(delta_time = "3 year"))
  expect_no_error(ModelConfig(delta_time = "10 d"))
  expect_no_error(ModelConfig(delta_time = "m"))
  expect_no_error(ModelConfig(delta_time = "we"))

})




test_that("'show' method prints out configuration as expected", {

  # default inputs (Density-informed mov model)
  expect_snapshot(ModelConfig())

  # CRW movement model - i.e. spatial resolution should be printed
  expect_snapshot(ModelConfig(movement_type = "crw"))

  # With starting-sites
  s <- sf::st_sf(
    id = c("A", "B", "C"),
    prop = c(0.30, 0.30, 0.40),
    geom = sf::st_sfc(sf::st_point(c(1,1)), sf::st_point(c(2,2)), sf::st_point(c(3,3))),
    crs = 4326
  )

  expect_snapshot(ModelConfig(start_sites = s))


  # With starting-sites and (n>10) end-sites
  e <- data.frame(
    x = seq(1, 9, length.out = 15),
    y = seq(9, 1, length.out = 15),
    prop = rep(1/15, 15),
    id = LETTERS[1:15]
  ) |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)

  expect_snapshot(ModelConfig(start_sites = s, end_sites = e))

  # UTM units
  expect_snapshot(ModelConfig(movement_type = "crw", ref_sys = sf::st_crs(32630)))

})




## Dev Testing ------------------------------------
test_that("Dev testing", {

  skip("ModelConfig show method testing")

  s <- sf::st_sf(
    id = c("A", "B", "C"),
    prop = c(0.30, 0.30, 0.40),
    geom = sf::st_sfc(sf::st_point(c(1,1)), sf::st_point(c(2,2)), sf::st_point(c(3,3))),
    crs = 4326
    #crs = 32630
  )

  s

  m <- ModelConfig(start_sites = s)
  m

  sf::st_crs(s)$IsGeographic
  sf::st_crs(s)$wkt



  s <- data.frame(
    x = seq(1, 9, length.out = 15),
    y = seq(9, 1, length.out = 15),
    prop = rep(1/15, 15),
    id = LETTERS[1:15]
  )

  sites <- sf::st_as_sf(s, coords = c("x", "y"), crs = 4326)

  ModelConfig(start_sites = sites)

  ModelConfig(ref_sys = sf::st_crs(NA), n_agents = NA_integer_, delta_y = NA_real_, aoc_bbx = c(1, NA, 2, 3))
  ModelConfig()

})

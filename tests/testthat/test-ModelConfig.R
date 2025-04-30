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


})

# POITIVE TESTING -------------------------------------------------------------

test_that("Raster dimensions classified as expected", {

  dt <- generate_mock_stars(time_name = "time", time_type = "posixt") |>
    stars::st_set_dimensions("x", names = "weird")

  out <- introspect_stars(dt)
  expect_equal(out$raster$names, c("weird", "y"))
  expect_equal(out$raster$dims, c(1, 2))

  # raster dimensions in unusual positions
  dt_repos <- aperm(dt, c(2, 3, 1))
  out <- introspect_stars(dt_repos)

  expect_equal(out$raster$names, c("weird", "y"))
  expect_equal(out$raster$dims, c(3, 1))


  # no non-raster dims
  out <- introspect_stars(generate_mock_stars())

  expect_equal(out$raster$names, c("x", "y"))
  expect_length(out$non_raster$names, 0)
  expect_null(out$non_raster)

})


test_that("Non-Raster Temporal Dimension classified as expected", {

  # <Posixt>
  dt <- generate_mock_stars(time_name = "spacetime", time_type = "posixt")
  out <- introspect_stars(dt)

  expect_equal(out$non_raster$dims, 3)
  expect_equal(out$non_raster$names, "spacetime")
  expect_equal(out$non_raster$types, "temporal")
  expect_equal(out$non_raster$procs, "nearest")
  expect_equal(out$non_raster$cls, "POSIXct")

  # <Date>
  dt <- generate_mock_stars(time_name = "Sampling day", time_type = "date")
  out <- introspect_stars(dt)

  expect_equal(out$non_raster$dims, 3)
  expect_equal(out$non_raster$names, "Sampling day")
  expect_equal(out$non_raster$types, "temporal")
  expect_equal(out$non_raster$cls, "Date")

  # <character> - Month
  dt <- generate_mock_stars(time_name = "t", time_type = "month")
  out <- introspect_stars(dt)

  expect_equal(out$non_raster$names, "t")
  expect_equal(out$non_raster$types, "temporal")
  expect_equal(out$non_raster$proc, "month_chr")
  expect_equal(out$non_raster$cls, "character")

  # <numeric> - year
  dt <- generate_mock_stars(time_name = "year", time_type = "numeric") |>
    stars::st_set_dimensions("year", values = 1990:1999)

  out <- introspect_stars(dt)

  expect_equal(out$non_raster$types, "temporal")
  expect_equal(out$non_raster$proc, "year")

  # <numeric> - month, quarter, yearweek, yearday
  purrr::walk2(
    c("month", "quarter", "yearday", "yearweek"),
    c("month_num", "quarter", "yday", "week"),
    function(x, y){
      dt <- generate_mock_stars(time_name = x, time_type = "numeric", time_length = 4)

      out <- introspect_stars(dt)

      expect_equal(out$non_raster$types, "temporal")
      expect_equal(out$non_raster$proc, y)
      expect_equal(out$non_raster$cls, "numeric")
    })


  # handles simple name deviations for numeric dimensions
  purrr::walk(
    c("Month", "QUARTER", "Year-Day", "year.Week", "months", "months"),
    function(dim_name){
      dt <- generate_mock_stars(time_name = dim_name, time_type = "numeric", time_length = 4)
      out <- introspect_stars(dt)

      expect_equal(out$non_raster$names, dim_name)
      expect_equal(out$non_raster$types, "temporal")
    }
  )

})



test_that("Non-Raster iteration dimension classified as expected", {

  purrr::walk(
    c("iter", "iteration", "sample", "boot", "bootstrap"),
    function(x){
      dt <- generate_mock_stars(iter_name = x, time_type = "numeric")
      out <- introspect_stars(dt)

      expect_equal(out$non_raster$types, "iteration")
      expect_equal(out$non_raster$proc, "draw")
      expect_equal(out$non_raster$cls, "numeric")
    })

})



test_that("Non-Raster Temporal AND Iteration Dimensions classified as expected", {

  # temporal AND iteration dimensions
  dt <- generate_mock_stars(time_name = "time", time_type = "posixt", iter_name = "iter") |>
    aperm(c(2, 4, 1, 3))

  out <- introspect_stars(dt)

  expect_equal(out$non_raster$names, c("iter", "time"))
  expect_equal(out$non_raster$dims, c(2, 4))
  expect_equal(out$non_raster$types, c("iteration", "temporal"))
  expect_equal(out$non_raster$proc, c("draw", "nearest"))
  expect_equal(out$non_raster$cls, c("numeric", "POSIXct"))

})





# NEGATIVE TESTING -------------------------------------------------------------
test_that("Validation works as expected", {

  # no more than one attribute
  dt <- generate_mock_stars(attr_name = "dens", attr_units = "counts/m2")
  dt$extra <- dt$dens * 10

  expect_error(
    introspect_stars(dt),
    "object with one single attribute",
    class = "err-Driver-init-nonsingle-attribute"
  )

  # No more than 4 dimensions
  dt <- generate_mock_stars(attr_units = "", time_name = "time", iter_name = "boot")
  dt <- c(dt, dt, along = 5)

  expect_error(
    introspect_stars(dt),
    "cannot have more than 4 dimensions.",
    class = "err-Driver-init-morethan4dims"
  )


  # Temporal character - invalid months
  dt <- generate_mock_stars(time_name = "t", time_type = "month") |>
    stars::st_set_dimensions("t", values = letters[1:10])

  expect_snapshot(error = TRUE, introspect_stars(dt))


  # Temporal numeric - invalid dim names
  dt <- generate_mock_stars(time_name = "t", time_type = "numeric") |>
    stars::st_set_dimensions("t", names = "WRONG_NAME")

  expect_snapshot(error = TRUE, introspect_stars(dt))
  expect_snapshot(error = TRUE, Driver(id = "a", stars_obj = dt))


  # Temporal numeric - negative values
  dt <- generate_mock_stars(time_name = "year", time_type = "numeric") |>
    stars::st_set_dimensions("year", values = -c(1:10))

  expect_snapshot(error = TRUE, introspect_stars(dt))
  expect_snapshot(error = TRUE, Driver(id = "a", stars_obj = dt))


  # Temporal numeric - non-integer numbers
  dt <- generate_mock_stars(time_name = "yearweek", time_type = "numeric", time_length = 3) |>
    stars::st_set_dimensions("yearweek", values = c(1.2, 3.4, 6))

  expect_snapshot(error = TRUE, introspect_stars(dt))
  expect_snapshot(error = TRUE, Driver(id = "a", stars_obj = dt))


  # Temporal numeric - year must be represented by 4-digit numbers
  dt <- generate_mock_stars(time_name = "year", time_type = "numeric", time_length = 2) |>
    stars::st_set_dimensions("year", values = c(22, 2))

  expect_snapshot(error = TRUE, introspect_stars(dt))
  expect_snapshot(error = TRUE, Driver(id = "a", stars_obj = dt))


  # Temporal numeric - values surpassing expected bounds
  purrr::walk(
    c("month", "quarter", "yearday", "yearweek"),
    function(dim_name){
      dt <- generate_mock_stars(time_name = dim_name, time_type = "numeric", time_length = 3) |>
        stars::st_set_dimensions(dim_name, values = c(400, 500, 600))

      expect_snapshot(error = TRUE, introspect_stars(dt))
      expect_snapshot(error = TRUE, Driver(id = "a", stars_obj = dt))

    }
  )


  # more than one temporal or iteration dimension
  dt <- generate_mock_stars(time_name = "time")
  dt <- c(dt, dt, along = 4) |>
    stars::st_set_dimensions("new_dim", names = "month", values = month.name[1:2])

  expect_snapshot(introspect_stars(dt))

  dt <- generate_mock_stars(iter_name = "iter")
  dt <- c(dt, dt, along = 4) |> stars::st_set_dimensions("new_dim", names = "boot", values = 1:2)

  expect_snapshot(introspect_stars(dt))


})





test_that("on-the-fly dev testing", {

  skip("dev testing")

  dt <- generate_mock_stars(time_name = "t", time_type = "numeric")

})



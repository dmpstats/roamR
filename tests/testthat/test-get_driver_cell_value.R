test_that("extracts expected cell value for raster-only datacubes", {

  # initiate an example agent
  a <- Agent()
  location(a) <- sf::st_point(c(2, 2))

  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(attr_name = "sst", attr_units = "degree_Celsius")
  )

  expected <- d@stars_obj |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)

})



test_that("extracts expected cell value for temporal-type dimensions", {

  date <- "2023-02-01"
  target_time <- as.POSIXct("2023-02-08 09:10:00")

  # initiate an example agent
  a <- Agent()
  location(a) <- sf::st_point(c(2, 2))


  # Posixt ----------------------------------------------

  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "sst",
      attr_units = "degree_Celsius",
      time_name = "t",
      time_type = "posixt",
      start_date = date
    )
  )

  # exact match
  a@condition@timestamp <- target_time

  expected <- d@stars_obj |>
    dplyr::filter(t == a@condition@timestamp) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)


  # approx to nearest precedent
  a@condition@timestamp <- target_time + 6557

  expected <- d@stars_obj |>
    dplyr::slice(t, 8) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)


  # Date ----------------------------------------------
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "t", time_type = "date",
      start_date = date)
    )

  # exact date match
  a@condition@timestamp <- target_time

  expected <- d@stars_obj |>
    dplyr::filter(t == as.Date(a@condition@timestamp)) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)


  # approx to nearest precedent date
  a@condition@timestamp <- target_time + lubridate::days(1)

  expected <- d@stars_obj |>
    dplyr::slice(t, 2) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)


  # Month character ----------------------------------------------
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "t", time_type = "month",
      start_date = date)
  )

  a@condition@timestamp <- target_time

  expected <- d@stars_obj |>
    dplyr::filter(t == "Feb") |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)


  # Numerics ----------------------------------------------

  ## months
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "month", time_type = "numeric",
      start_date = date)
  )

  a@condition@timestamp <- target_time

  expected <- d@stars_obj |>
    dplyr::filter(month == 2) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)


  ## years
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "year", time_type = "numeric",
      start_date = date) |>
      stars::st_set_dimensions("year", values = 2015:2025)
  )

  a@condition@timestamp <- target_time

  expected <- d@stars_obj |>
    dplyr::filter(year == 2023) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)


  ## quarters
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "quarter", time_type = "numeric",
      start_date = date, time_length = 4)
  )

  a@condition@timestamp <- target_time

  expected <- d@stars_obj |>
    dplyr::filter(quarter == 1) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)

  ## weeks
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "yearweek", time_type = "numeric",
      start_date = date)
  )

  a@condition@timestamp <- target_time

  expected <- d@stars_obj |>
    dplyr::filter(yearweek == lubridate::week(a@condition@timestamp)) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)


  ## days of year
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "yearday", time_type = "numeric",
      start_date = date, time_length = 50)
  )

  a@condition@timestamp <- target_time

  expected <- d@stars_obj |>
    dplyr::filter(yearday == lubridate::yday(a@condition@timestamp)) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  expect_equal(get_driver_cell_value(d, a), expected)

})





test_that("extracts expected cell value for iteration-type dimensions", {

  a <- Agent()
  location(a) <- sf::st_point(c(2, 2))

  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      iter_name = "iter"
    )
  )

  set.seed(10)
  expected <- d@stars_obj |>
    dplyr::filter(iter == sample(10, 1)) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  set.seed(10)
  expect_equal(get_driver_cell_value(d, a), expected)


  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      iter_name = "boot"
    )
  )

  set.seed(10)
  expected <- d@stars_obj |>
    dplyr::filter(boot == sample(10, 1)) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  set.seed(10)
  expect_equal(get_driver_cell_value(d, a), expected)

})



test_that("extracts expected cell value for temporal-type AND iteration-type dimensions", {

  date <- "2023-02-01"
  target_time <- as.POSIXct("2023-02-08 09:10:00")

  # initiate an example agent
  a <- Agent()
  location(a) <- sf::st_point(c(2, 2))
  a@condition@timestamp <- target_time

  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "sst",
      attr_units = "degree_Celsius",
      time_name = "t",
      time_type = "posixt",
      iter_name = "sample",
      start_date = date
    )
  )

  set.seed(10)
  expected <- d@stars_obj |>
    dplyr::filter(
      t ==  a@condition@timestamp,
      sample == sample(10, 1)
    ) |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull()

  set.seed(10)
  expect_equal(get_driver_cell_value(d, a), expected)

})




test_that("returns NA if agents data outside range of temporal-type dimension", {

  date <- "2023-02-01"
  target_time <- as.POSIXct("2023-02-08 09:10:00")

  # initiate an example agent
  a <- Agent()
  location(a) <- sf::st_point(c(2, 2))

  ## day of year
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "yearday", time_type = "numeric",
      start_date = date, time_length = 50)
  )

  a@condition@timestamp <- target_time - lubridate::days(100)
  expect_true(is.na(get_driver_cell_value(d, a)))


  ## month character
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "m", time_type = "month",
      start_date = date, time_length = 3)
  )

  a@condition@timestamp <- target_time + lubridate::days(90)
  expect_true(is.na(get_driver_cell_value(d, a)))


  # Posixt
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "m", time_type = "posixt",
      start_date = date, time_length = 3)
  )

  a@condition@timestamp <- target_time + lubridate::days(10)
  expect_true(is.na(get_driver_cell_value(d, a)))


  # Date
  d <- Driver(
    id = "d",
    stars_obj = generate_mock_stars(
      attr_name = "s", attr_units = "m",
      time_name = "m", time_type = "date",
      start_date = date, time_length = 3)
  )

  a@condition@timestamp <- target_time - lubridate::days(10)
  expect_true(is.na(get_driver_cell_value(d, a)))


})



test_that("ignores vector field attributes, when present", {

  d <- Driver(id = "d", stars_obj = generate_mock_stars(attr_name = "sst", attr_units = "g"))
  stars_obj(d)$slope <- {stars_obj(d)$sst * runif(25)} |> units::set_units("kg")

  a <- Agent()
  location(a) <- sf::st_point(c(2, 2))

  expected <- d@stars_obj |>
    stars::st_extract(at = sf::st_coordinates(location(a))) |>
    dplyr::pull(sst)

  expect_equal(get_driver_cell_value(d, a), expected)

})





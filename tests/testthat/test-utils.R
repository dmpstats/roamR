test_that("`is_stars_empty()` behaves as expected", {

  expect_true(is_stars_empty(stars::st_as_stars(matrix(NA))))

  expect_false(is_stars_empty(stars::st_as_stars(matrix(0))))
  expect_false(is_stars_empty(rover_drivers$drv_sss@stars_obj))

  expect_false(
    rover_drivers$drv_sss@stars_obj |>
    split("months") |>
      is_stars_empty()
    )

  expect_error(
    is_stars_empty(1),
    regexp = "Argument `x` must be of class",
    class = "err-arg-wrong-class"
  )

})

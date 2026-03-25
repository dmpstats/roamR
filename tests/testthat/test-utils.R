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


# `set_fn_env()` -----------------------------------------------------------------

test_that("set_fn_env captures a single local dependency", {


  # Define a dependency in the Global Environment
  local_helper <- function(x) x + 1
  # Define a function that uses it
  main_fn <- function(y) local_helper(y)
  # Isolate the function
  portable_fn <- set_fn_env(main_fn)

  # Remove the original helper to prove it's captured
  rm(local_helper)

  # Assertions
  expect_equal(portable_fn(10), 11)
  expect_identical("local_helper", names(environment(portable_fn)))

})



test_that("set_fn_env handles nested local dependencies", {

  inner_inner_fn <- function(a) a * 2
  inner_fn <- function(b) inner_inner_fn(b) / 10
  outer_fn <- function(c) inner_fn(c) + 5

  portable_outer <- set_fn_env(outer_fn)

  # Check if the recursive search found `inner_inner_fn`
  env_objs <- ls(environment(portable_outer))
  expect_contains(env_objs, "inner_inner_fn")

  # check if upper level `inner_fn` was also found and injected it into
  # `portable_outer`'s env
  expect_contains(env_objs, "inner_fn")

  # check if original and portable function prodeuce identical outputs
  expect_identical(portable_outer(2), outer_fn(2))

})



test_that("set_fn_env ignores package-loaded functions", {
  # purrr::map is in a package environment, not .GlobalEnv
  fn_with_pkg <- function(x) purrr::map(x, ~ .x + 1)

  portable_pkg_fn <- set_fn_env(fn_with_pkg)

  # Assertions: 'map' should NOT be identified as local dependency and
  # therefore not bound to portable function
  expect_false("map" %in% ls(environment(portable_pkg_fn)))
  # But it should still work because caller_env() is the parent
  expect_equal(unlist(portable_pkg_fn(1:2)), c(2, 3))
})





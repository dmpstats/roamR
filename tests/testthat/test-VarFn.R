test_that("VarFn() defaults to 'empty' <VarFn>", {

  out <- VarFn()

  expect_true(is_empty_function(out@fn))
  expect_true(length(out@args_spec) == 0)
  expect_true(nchar(out@units) == 0)
  expect_true(is_empty_function(out@fn_cmp))

})




test_that("VarFn() fails when expected", {

  myfn <- function(sst, alpha) sst*alpha

  expect_error(
    VarFn(myfn, list(beta = 1, alpha = 2)),
    "All formal arguments of"
  )

  expect_error(
    VarFn(myfn, list(sst = list(), alpha = "dri")),
    "Invalid elements listed in"
  )

  expect_error(
    VarFn(myfn, list(sst = Driver(), alpha = "body_mass")),
    "Invalid elements listed in"
  )

  expect_error(
    VarFn(myfn, list(sst = TRUE, alpha = "body_mass")),
    "Invalid elements listed in"
  )

  expect_error(
    VarFn(myfn, list(sst = NA, alpha = "body_mass")),
    "Invalid elements listed in"
  )

})




test_that("VarFn() works as expected", {

  myfn <- function(sst = 2, alpha) sst*alpha

  expect_s4_class(
    VarFn(
      myfn,
      list(
        sst = VarDist(distr = distributional::dist_bernoulli(0.2)),
        alpha = "driver"
      )),
    class = "VarFn"
  )


  expect_no_error(
    VarFn(myfn, list(sst = "driver", alpha = "time_at_state"))
  )

})



test_that("args_spec list elements are named as expected", {

  myfn <- function(sst = 2, alpha) sst*alpha

  # unnamed elements of type <ArgSpec> are named according to ArgSpec@name
  expect_named(
    VarFn(myfn, args_spec = list(ArgSpec("sst"), ArgSpec("alpha"))) |>
      slot("args_spec"),
    expected = c("sst", "alpha")
  )

  expect_named(
    VarFn(myfn, args_spec = list(ArgSpec("sst"), alpha = "time_at_state")) |>
      slot("args_spec"),
    expected = c("sst", "alpha")
  )


  # named elements of type <ArgSpec> are forcibly renamed according to ArgSpec@name
  expect_named(
    VarFn(myfn, args_spec = list(ArgSpec("sst"), x = ArgSpec("alpha"))) |>
      slot("args_spec"),
    expected = c("sst", "alpha")
  )


  # unnamed elements of type other than <ArgSpec> are named according to fn's arguments, by position
  expect_named(
    VarFn(myfn, args_spec = list("driver", "body_mass")) |>
      slot("args_spec"),
    expected = rlang::fn_fmls_names(myfn)
  )

  expect_named(
    VarFn(myfn, args_spec = list(sst = "driver", "body_mass")) |>
      slot("args_spec"),
    expected = rlang::fn_fmls_names(myfn)
  )

})

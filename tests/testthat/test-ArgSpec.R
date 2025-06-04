test_that("ArgSpec() works", {
  expect_no_error(ArgSpec(name = "i", type = "constant", value = 2))
})



test_that("Given inputs, ArgSpec() fails as expected", {

  # `name`
  expect_error(ArgSpec(type = "d"), "`name` is absent but must be supplied")
  expect_error(ArgSpec(name = 1), "Argument `name` must be an object of class")

  # `type`""
  expect_error(ArgSpec("x", type = "d"), "`type` must be one of")
  expect_error(ArgSpec("x", type = 1),"`type` must be a character vector")

  expect_error(ArgSpec("x", type = "random"), "`distr` must be provided when")
  expect_error(ArgSpec("x", type = "constant"), "`value` must be provided when")

  expect_error(ArgSpec("x", type = "time_at_state"), "`state_id` must be provided when")

})



test_that("Argument dependencies: warnings issued when expected", {

  dst <- distributional::dist_bernoulli(0.1)
  expect_warning(ArgSpec("x", type = "driver", distr = dst))
  expect_warning(ArgSpec("x", type = "constant", value = 3, distr = dst))

  expect_no_warning(ArgSpec("x", type = "constant", value = 3))
  expect_no_warning(ArgSpec("x", type = "body_mass"))

})




test_that("Argument dependencies work as expected", {

  # `driver_id`
  ## takes `name` if missing
  out <- ArgSpec(name = "arg1", type = "driver")
  expect_equal(out@name, out@driver_id)

  # if specified
  out <- ArgSpec(name = "arg1", type = "driver", driver_id = "driver_name")
  expect_equal(out@driver_id, "driver_name")
  expect_true(out@name != out@driver_id)

  # @driver_id returned as NA if any other `type`
  out <- ArgSpec(name = "arg1", type = "body_mass", driver_id = "driver_name")
  expect_true(is.na(out@driver_id))
  out <- ArgSpec(name = "arg1", type = "body_mass")
  expect_true(is.na(out@driver_id))


  # `distr`
  ## returns error if `distr` is missing when type is "random"
  expect_error(
    ArgSpec(name = "arg1", type = "random"),
    "`distr` must be provided"
  )

  ## @distr returned as "dist_missing" if type is other
  expect_warning(out <- ArgSpec(name = "arg1", type = "body_mass", distr = distributional::dist_bernoulli(0.1)))
  expect_true(is.na(out@distr))


  # `units`
  ## defaults to "grams" and "minutes" when type is "body_mass" and "time_at_state", respectively
  out <- ArgSpec(name = "arg1", type = "body_mass")
  expect_equal(out@units, "g")
  out <- ArgSpec(name = "arg1", type = "time_at_state", state_id = "bla")
  expect_equal(out@units, "min")

  ## but take user-defined value if specified
  out <- ArgSpec(name = "arg1", type = "body_mass", units = "kg")
  expect_equal(out@units, "kg")
  out <- ArgSpec(name = "arg1", type = "time_at_state", state_id = "bla", units = "days/week")
  expect_equal(out@units, "days/week")

})





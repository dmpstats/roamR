test_that("State() creates an S4 <State> object", {
  expect_s4_class(
    object = State(),
    "State"
  )
})


# negative testing
test_that("State() throws an error when invalid input is provided", {
  # id should be a character string
  expect_snapshot(
    error = TRUE,
    State(id = 123) # id should be a character string
  )

  # type should be one of the specified categories
  expect_snapshot(
    error = TRUE,
    State(type = "invalid_type")
  )

  # type should be a single string
  expect_snapshot(
    error = TRUE,
    State(type = c("foraging", "sleeping"))
  )

  # energy_cost should be a VarDist object
  expect_snapshot(
    error = TRUE,
    State(energy_cost = "not_a_VarDist")
  )

  # time_budget should be a VarDist object
  expect_snapshot(
    error = TRUE,
    State(time_budget = "not_a_VarDist")
  )

  # speed should be a VarDist object
  expect_snapshot(
    error = TRUE,
    State(speed = "not_a_VarDist")
  )
})

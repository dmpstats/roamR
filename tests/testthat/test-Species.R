test_that("Species() creates an <Species> S4 object", {
  expect_s4_class(Species(), class = "Species")
})





test_that("Validation on time budget exchangeability across states works as expected", {

  # same units
  expect_no_error(
    Species(states_profile = list(
      a = State("a", time_budget = VarDist(1, "hours/day")),
      b = State("b", time_budget = VarDist(2, "hours/day"))
    ))
  )

  # convertible units
  expect_no_error(
    Species(states_profile = list(
      a = State("a", time_budget = VarDist(1, "hours/day")),
      b = State("b", time_budget = VarDist(2, "minutes/day")),
      c = State("c", time_budget = VarDist(3, "minutes/hour"))
    ))
  )

  # unitless
  expect_no_error(
    Species(states_profile = list(
      a = State("a", time_budget = VarDist(1, "")),
      b = State("b", time_budget = VarDist(2, ""))
    ))
  )


  expect_error(
    Species(states_profile = list(
      a = State("a", time_budget = VarDist(1, "hours/day")),
      b = State("b", time_budget = VarDist(2, "grams/day"))
    ))
  )

})

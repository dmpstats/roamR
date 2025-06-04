test_that("AgentCondition() creates an S4 <AgentCondition> object", {
  expect_s4_class(
    object = AgentCondition(),
    "AgentCondition"
  )
})



test_that("AgentCondition() works as expected", {

  expect_no_error(AgentCondition(states_budget = list(a = 0.3,b = 0.5,c = 0.2)))

  expect_no_error(
    AgentCondition(
      states_budget = list(
        a = units::set_units(0.3, ""),
        b = units::set_units(0.5, ""),
        c = units::set_units(0.2, "")
      ))
  )

})

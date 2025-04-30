test_that("returns an <Agent> S4 object", {
  expect_s4_class(Agent(), class = "Agent")
})




test_that("works as expected", {

  out <- Agent(species = rover, model_config = ibm_config_rover)
  expect_s4_class(out@condition, "AgentCondition")
  expect_s4_class(out@properties, "AgentProperties")

  out@history
})

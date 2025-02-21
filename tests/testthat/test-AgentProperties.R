test_that("AgentProperties() creates an S4 <AgentProperties> object", {
  expect_s4_class(
    object = AgentProperties(),
    "AgentProperties"
  )
})


test_that("BehaviourSpec() creates an S4 <BehaviourSpec> object", {
  expect_s4_class(
    object = BehaviourSpec(),
    "BehaviourSpec"
  )
})

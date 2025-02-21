test_that("State() creates an S4 <State> object", {
  expect_s4_class(
    object = State(),
    "State"
  )
})

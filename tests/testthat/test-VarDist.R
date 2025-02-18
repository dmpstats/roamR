test_that("VarDist() creates a <VarDist> S4 object", {
  expect_s4_class(VarDist(), class = "VarDist")
})


test_that("Variable get constant values if numeric is passed to `distr`", {
  expect_no_error(VarDist(10))
  expect_true(distributional::variance(VarDist(10)@distr) == 0)
})




test_that("Validation on slot lengths works as expected", {

  # numeric `distr`
  expect_error(VarDist(distr = c(1, 2)), class = "err-arg-wrong-length")

  # distributional `distr`
  expect_error(
    VarDist(distr = distributional::dist_bernoulli(c(0.1, 0.2))),
    class = "err-arg-wrong-length"
  )

  expect_error(
    VarDist(units = c("m", "c")),
    class = "err-arg-wrong-length"
  )

})



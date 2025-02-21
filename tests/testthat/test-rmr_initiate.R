test_that("rmr_initiate() creates a <IBM> S4 object", {
  out <- rmr_initiate(ModelConfig(), Species(), Driver(), verbose = FALSE)
  expect_s4_class(out, class = "IBM")
})


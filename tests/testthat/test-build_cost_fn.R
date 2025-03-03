test_that("a function is returned", {
  expect_true(
    is.function(
    build_cost_fn(\(b) b, drivers = list(Driver()))
    )
  )
})




test_that("build_cost_fn() fails when base function has invalid arguments", {

  # non-allowed arguments
  basefoo <- function(b, w, t, r) b + w * t - r
  expect_error(
    build_cost_fn(basefoo, list(Driver())),
    class = "err-invalid-base-fn-arg"
  )


  # driver without a stars object
  land_cost <- function(land) land^2
  final_fn <- build_cost_fn(land_cost, rover_drivers)
  a <- Agent(rover, ibm_config_rover)
  expect_error(final_fn(rover_drivers, a))

})






test_that("build_cost_fn() works as expected", {

  a <- Agent(rover, ibm_config_rover)

  # one argument
  one_arg_cost <- function(sst){
    113-(2.75*sst)
  }

  final_fn <- build_cost_fn(one_arg_cost, rover_drivers)
  final_fn(rover_drivers, a)

  #bench::mark(final_fn(rover_drivers, a), iterations = 1000)

  # two arguments
  two_args_cost <- function(sst, b){
    if(sst < 5){
      113-(2.75*sst)
    }else{
      113-(2.75*sst) * b
    }
  }

  final_fn <- build_cost_fn(two_args_cost, rover_drivers)
  final_fn(rover_drivers, a)

  # only biomass
  cost_bio <- function(b)  113-(2.75*b)
  final_fn <- build_cost_fn(cost_bio, rover_drivers)
  final_fn(rover_drivers, a)

  #bench::mark(final_fn(rover_drivers, a))


  uncertaint_cost <- function(b){
    intcp <- rnorm(1, 113, 10)
    intcp/(2.75*b)
  }

  final_fn <- build_cost_fn(uncertaint_cost, rover_drivers)
  final_fn(rover_drivers, a)



})






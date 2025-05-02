test_that("dev testing", {

  skip("developing puposes only")

  # initialize ibm for mock sepcies
  ibm <- rmr_initiate(ibm_config_rover, rover, rover_drivers)

  rmr_run(ibm, .parallel_plan = "multicore")



})

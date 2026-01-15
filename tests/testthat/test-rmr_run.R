test_that("dev testing", {

  skip("developing purposes only")

  # initialize ibm for mock species
  x <- rmr_initiate(ibm_config_rover, rover, rover_drivers)


  rmr_run(x, future::plan(future::multicore))

  rmr_run(ibm)

  x@drivers$drv_land@id

})

test_that("a function is returned", {
  expect_true(
    is.function(
    build_cost_fn(VarFn())
    )
  )
})



test_that("application to single-argument function works as expected", {

  a <- Agent(rover, ibm_config_rover)
  location(a) <- location(a) + 1.5 # hack: relocate to get non-NA SST value

  # dependent on driver - SST
  usr_fn <- function(sst){ 113-(2.75*sst) }
  state_cost <- VarFn(fn = usr_fn, args_spec = list(sst = "driver"))

  mnf_fn <- build_cost_fn(state_cost)

  expected <- rover_drivers$drv_sst@stars_obj |>
    dplyr::filter(months == "December") |>
    stars::st_extract(sf::st_coordinates(location(a))) |>
    as.numeric() |>
    #units::drop_units() |>
    usr_fn()


  output <- mnf_fn(a, rover_drivers) |> as.numeric()
  expect_equal(output, expected)


  # dependent on body_mass
  usr_fn <- function(b){ b^2 }
  state_cost <- VarFn(fn = usr_fn, args_spec = list(b = "body_mass"))

  mnf_fn <- build_cost_fn(state_cost)

  expect_equal(
    mnf_fn(a, rover_drivers),
    usr_fn(body_mass(a)) |> units::drop_units()
  )


  # dependent on a "random" term - alpha
  usr_fn <- function(alpha){ alpha + 10 }
  alpha_dist <- distributional::dist_normal(5, 1)

  snooze_cost <- VarFn(
    fn = usr_fn,
    args_spec = list(alpha = VarDist(alpha_dist))
  )

  r <- rover # make a copy of the example species
  r@states_profile$snooze <- State(id = "snooze", energy_cost = snooze_cost) # add the snooze state

  a <- Agent(r, ibm_config_rover) # initiate agent
  alpha_draw <- a@properties@cost_par_draws$snooze$alpha |> units::drop_units() # extract the value drawn from alpha_dist

  mnf_fn <- build_cost_fn(snooze_cost, state_id = "snooze")

  expect_equal(
    mnf_fn(a, rover_drivers),
    usr_fn(alpha_draw)
  )


  # dependent on a "constant" term - eta
  usr_fn <- function(eta){ sqrt(eta) }

  state_cost <- VarFn(
    fn = usr_fn,
    args_spec = list(ArgSpec(name = "eta", "constant", value = 6))
  )

  mnf_fn <- build_cost_fn(state_cost, rover_drivers)

  output <- mnf_fn(a, rover_drivers, details = TRUE)

  expect_equal(output$input$eta, 6)
  expect_equal(output$output, usr_fn(6) )

})






test_that("application to multi-argument works as expected", {

  a <- Agent(rover, ibm_config_rover) # initiate agent
  location(a) <- location(a) + 2 # hack: relocate to get non-NA SST value

  deg <- rover_drivers$drv_sst@stars_obj |>
    dplyr::filter(months == "December") |>
    stars::st_extract(sf::st_coordinates(location(a))) |>
    units::drop_units() |>
    as.numeric()

  mass <- body_mass(a) |> units::drop_units()


  # two arguments
  usr_fn <- function(sst, b){ 113-(2.75*sst) + b }

  state_cost <- VarFn(
    fn = usr_fn,
    args_spec = list(
      sst = "driver",
      b = "body_mass")
  )

  mnf_fn <- build_cost_fn(state_cost, rover_drivers)

  expect_equal(
    mnf_fn(a, rover_drivers),
    usr_fn(deg, mass)
  )

})




test_that("application to multiple random arguments works as expected", {

  # Four arguments - with random intercept 'int' and random alpha
  usr_fn <- function(sst, b, int, alpha){ int - (2.75*sst) + b/alpha }

  int_dist <- distributional::dist_uniform(100, 200)
  alpha_dist <- distributional::dist_uniform(0.1, 10)

  snooze_cost <- VarFn(
    fn = usr_fn,
    args_spec = list(
      sst = "driver",
      b = "body_mass",
      alpha = VarDist(alpha_dist),
      int = VarDist(int_dist)
    )
  )

  r <- rover # make a copy of the example species
  r@states_profile$snooze <- State(id = "snooze", energy_cost = snooze_cost) # add the snooze state, below

  a <- Agent(r, ibm_config_rover) # initiate agent
  location(a) <- location(a) + 2 # hack: relocate to get non-NA SST value

  deg <- rover_drivers$drv_sst@stars_obj |>
    dplyr::filter(months == "December") |>
    stars::st_extract(sf::st_coordinates(location(a))) |>
    units::drop_units() |>
    as.numeric()

  mass <- body_mass(a) |> units::drop_units()


  alpha_draw <- a@properties@cost_par_draws$snooze$alpha |> units::drop_units() # extract the value drawn from alpha_dist
  int_draw <- a@properties@cost_par_draws$snooze$int |> units::drop_units() # extract the value drawn from int_dist

  mnf_fn <- build_cost_fn(snooze_cost, state_id = "snooze")

  #rlang::env_print(mnf_fn)$vrf
  expect_equal(
    mnf_fn(a, rover_drivers),
    usr_fn(sst = deg, b =mass, alpha = alpha_draw, int = int_draw)
  )

})



test_that("application to multi-argument works as expected", {

  # user-function expects SST in Farenheight
  usr_fn <- function(sst, b){ 113-(2.75*sst) + b }

  state_cost <- VarFn(
    fn = usr_fn,
    args_spec = list(
      sst = ArgSpec(name = "sst", type = "driver", units = "Fahrenheit"),
      b = "body_mass")
  )


  a <- Agent(rover, ibm_config_rover) # initiate agent
  location(a) <- location(a) + 2 # hack: relocate to get non-NA SST value

  deg <- rover_drivers$drv_sst@stars_obj |>
    dplyr::filter(months == "December") |>
    stars::st_extract(sf::st_coordinates(location(a))) |>
    dplyr::pull(1) |>
    units::set_units("Fahrenheit") |>  # convert Celsius to Fahren
    units::drop_units() # then drop units to use in user-function

  mass <- body_mass(a) |> units::drop_units()

  # Manufactured function should deal with units conversion internally
  mnf_fn <- build_cost_fn(state_cost, rover_drivers)

  expect_equal(
    mnf_fn(a, rover_drivers),
    usr_fn(deg, mass)
  )

})






test_that("fails as expected", {

  r <- rover # make a copy of the example species
  a <- Agent(r, ibm_config_rover)

  usr_fn <- function(d){ 113-(2.75*d) }
  state_cost <- VarFn(
    fn = usr_fn,
    args_spec = list(ArgSpec("d", type = "random", distr = distributional::dist_burr(1, 1)))
  )

  mnf_fn <- build_cost_fn(state_cost, state_id = "state")

  expect_error(
    mnf_fn(a, rover_drivers),
    "Can't find energy cost parameter for state ID"
  )


})












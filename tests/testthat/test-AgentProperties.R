test_that("AgentProperties() creates an S4 <AgentProperties> object", {
  expect_s4_class(
    object = AgentProperties(),
    "AgentProperties"
  )
})



test_that("slot-based specification works as expected", {

  spp <- "roamer"
  b <- units::set_units(13, "kg")
  s <- units::set_units(13, "m/s")
  p <- sf::st_point(c(1, 2))
  m <- units::set_units(10, "g")

  out <- AgentProperties(
    species_id = spp,
    initial_mass = b,
    speeds = list(swim = s),
    start_point = p,
    mortality_thresh = m
  )

  expect_identical(
    list(out@species_id, out@initial_mass, out@speeds$swim, out@start_point, out@mortality_thresh),
    list(spp, b, s, p, m)
  )

  expect_true(sf::st_is_empty(out@end_point))
  expect_true(length(out@move_influences) == 0)
  expect_true(is.na(out@age))
  expect_true(is.na(out@sex))

})



test_that("species-based specification works as expected", {

  library(units)
  library(distributional)
  library(purrr)

  out <- AgentProperties(species = rover, model_config = ibm_config_rover)

  expect_identical(out@species_id, rover@id)

  ci <- hilo(rover@body_mass_distr@distr, 99)
  ci_lwr <- set_units(ci$lower, rover@body_mass_distr@units, mode = "standard")
  ci_upp <- set_units(ci$upper, rover@body_mass_distr@units, mode = "standard")
  expect_true(out@initial_mass >= ci_lwr & out@initial_mass <= ci_upp)


  ci <- hilo(rover@states_profile$flight@speed@distr, 99)
  ci_lwr <- set_units(ci$lower, rover@states_profile$flight@speed@units, mode = "standard")
  ci_upp <- set_units(ci$upper, rover@states_profile$flight@speed@units, mode = "standard")
  expect_true(out@speeds$flying >= ci_lwr & out@speeds$flying <= ci_upp)

  ci <- hilo(rover@states_profile$swimming@speed@distr, 99)
  ci_lwr <- set_units(ci$lower, rover@states_profile$swimming@speed@units, mode = "standard")
  ci_upp <- set_units(ci$upper, rover@states_profile$swimming@speed@units, mode = "standard")
  expect_true(out@speeds$swimming >= ci_lwr & out@speeds$swimming <= ci_upp)

  # TODO
  # out@move_influences
  # out@costings
  # out@start_point
  # out@mortality_thresh

  out <- AgentProperties(species = rover, model_config = ibm_config_rover)

})

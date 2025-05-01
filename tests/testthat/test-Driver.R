test_that("generates S4 class <Driver>", {
  expect_s4_class(Driver(), "Driver")
})


test_that("Bare call generates empty <Driver>", {
  expect_true(is_empty(Driver()))
})



# High-level input validation
test_that("Constructor-level input validation works as expected", {

  dt_dens <- generate_mock_stars(attr_name = "dens", attr_units = "counts/m2")

  # wrong option for `obj_active`
  expect_error(
    Driver(id = "a", stars_obj = dt_dens, obj_active = "sfe")
  )

  # wrong  classes
  expect_error(Driver(id = "a", sf_obj = 3))
  expect_error(Driver(id = "a", stars_obj = 3))

  expect_no_error(Driver(id = "a", obj_active = "none"))
})



# Low-level <Driver> class validation
test_that("Class-level input validation works as expected", {

  dt_dens <- generate_mock_stars(attr_name = "dens", attr_units = "counts/m2")

  # `@obj_active` options
  expect_error(
    new("Driver", id = "a", stars_obj = dt_dens, obj_active = "sfe"),
    '@obj_active must take one of:'
  )

  # inconsistency between `@obj_active` and populated slots
  expect_error(
    Driver(id = "a", obj_active = "stars"),
    '@stars_obj must be non-empty when @obj_active is '
  )

  expect_error(
    Driver(id = "a", stars_obj = dt_dens, obj_active = "sf"),
    '@sf_obj must be populated with non-empty geometries when @obj_active is'
  )

  expect_no_error(
    Driver(id = "a", stars_obj = dt_dens, obj_active = "none")
  )

  # @stars_obj: attributes cannot be uniteless
  dt <- generate_mock_stars()
  expect_error(
    Driver(id = "a", stars_obj = dt),
    "- @stars_obj: attributes must have units. Attribute"
  )

})

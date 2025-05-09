# helper to clip <stars> datacubes based on polys
clip_holes <- function(x, y){

  holes <- sf::st_crop(x, y, as_points = FALSE)

  holes_pts <- holes |>
    sf::st_as_sf(as_points = TRUE) |>
    sf::st_geometry()

  holes_cells_idx <- stars::st_cells(x, holes_pts)

  x_holed <- stars::st_apply(x, 3, function(.x){
    .x[holes_cells_idx] <- NA
    .x
  })

  x_holed
}

# Drivers ------------------------------------------------------------

## Monthly density surfaces ----
dens <- stars_obj(drv_sp_distr) |> dplyr::slice(iter, 1)

plot(dens)

drv_dens <- Driver(
  id = "dens",
  type = "resource",
  descr = "Density",
  stars_obj = dens,
  stars_descr = "Monthly density maps",
  obj_active = "stars"
)



## impacted density surfaces ------
# For testing purposes, apply truncation-type impact, i.e. densities in clipped
# areas are not redistributed
imp_dens <- clip_holes(dens, owf_foots |> sf::st_buffer(20000) ) |>
  dplyr::mutate(counts = units::set_units(counts, "counts"))

plot(imp_dens)


drv_imp_dens <- Driver(
  id = "imp_dens",
  type = "resource",
  descr = "Impacted Density",
  stars_obj = imp_dens,
  stars_descr = "Monthly impacted density maps",
  obj_active = "stars"
)




## Monthly energy intake surfaces ------
energy_intake <- stars_obj(drv_prey) |>
  dplyr::slice(month, c(1, 2, 3, 4)) |>
  setNames("intake") |>
  dplyr::mutate(
    intake = units::drop_units(intake),
    intake = units::set_units(intake, "kJ/hr")
  )

plot(energy_intake)

drv_intake <- Driver(
  id = "intake",
  type = "resource",
  descr = "Energy Intake",
  stars_obj = energy_intake,
  stars_descr = "Monthly energy intake maps",
  obj_active = "stars"
)



## Monthly impacted energy intake surfaces ------
# impact applied randomly, i.e. not spatially explicit nor relative to location of impacts
imp_energy_intake <- stars::st_apply(energy_intake, c(1, 2, 3), \(x) x * runif(1, 0.4, 0.8)) |>
  dplyr::mutate(intake = units::set_units(intake, "kJ/hr"))

plot(imp_energy_intake)

drv_imp_intake <- Driver(
  id = "imp_intake",
  type = "resource",
  descr = "Impacted Energy Intake",
  stars_obj = imp_energy_intake,
  stars_descr = "Monthly impacted energy intake maps",
  obj_active = "stars"
)


# monthly SST maps
drv_sst


# Species -----------------------------------------------------------------

resp_dens <- DriverResponse(
  driver_id = "dens",
  movement = MoveInfluence(
    prob = VarDist(distributional::dist_degenerate(1)),
    type = "attraction",
    mode = "cell-value",
    sim_stage = "baseline"
  )
)

resp_imp_dens <- DriverResponse(
  driver_id = "imp_dens",
  movement = MoveInfluence(
    prob = VarDist(distributional::dist_normal(0.67, sd = 0.061)),
    type = "attraction",
    mode = "cell-value",
    sim_stage = "impact"
  )
)


spp <- Species(
  id = "rover",
  common_name = "Rover",
  scientific_name = "Rover Vulgaris",
  body_mass_distr = VarDist(distributional::dist_normal(1000, 0.2 * 1000), units = "grams"),
  mortality_thresh_distr = VarDist(distributional::dist_uniform(300, 350), units = "grams"),
  states_profile = rvr_states,
  driver_responses = list(
    resp_dens,
    resp_imp_dens
  )
)



# IBM object -----------------------------------------------------------------
rover_ibm_disnbs <- rmr_initiate(
  model_config = ibm_config_rover,
  species = spp,
  drivers = list(
    drv_dens,
    drv_intake,
    drv_imp_dens,
    drv_imp_intake,
    drv_sst
  )
)

usethis::use_data(rover_ibm_disnbs, overwrite = TRUE, compress = "xz")






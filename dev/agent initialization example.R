library(sf)
library(dplyr)
library(distributional)

# IBM settings -------------------------------------------------

isle_may <- st_sf(
  id = "Isle of May",
  prop = 1,
  geom = st_sfc(st_point(c(-2.5667, 56.1833))),
  crs = 4326
)

# UTM zone 30N
utm30 <- st_crs(32630)

# IBM Settings - modify parameter values to your specific case
guill_imb_config <- ModelConfig(
  n_agents = 1000,
  ref_sys = utm30,
  aoc_bbx = isle_may |> st_transform(utm30) |> st_buffer(2.5e5) |>  st_bbox(), #
  delta_x = 1000,
  delta_y = 1000,
  time_step = "1 day",
  start_date = Sys.Date(),
  end_date = Sys.Date() + 180, # ~6 months
  start_sites = isle_may |> st_transform(32630)
)



# States specification -------------------------------------------------

states <- list(
  flight = State(
    id = "flight",
    energy_cost = VarDist(dist_normal(141, 66), units = "kJ/hour/gram"), # vals from excel sheet. Normal not adequate as it will generate negative values - needs reparameterization to logN
    time_budget = VarDist(dist_uniform(0, 0.15), "hours/day"), # roughly based on Lila's summaries
    speed = VarDist(dist_uniform(10, 20), "m/s") # made-up
  ),
  dive = State(
    id = "diving",
    energy_cost = VarDist(dist_normal(200, 10), units = "kJ/hour/gram"), # made-up
    time_budget = VarDist(dist_uniform(2.5, 3.5), "hours/day"), # roughly based on Lila's summaries
    speed = VarDist(dist_uniform(10, 20), "m/s") # made-up
  ),
  active = State(
    id = "active_on_water",
    energy_cost = VarDist(dist_normal(150, 10), units = "kJ/hour/gram"), # made-up
    time_budget = VarDist(dist_uniform(10, 11), "hours/day"), # roughly based on Lila's summaries
    speed = VarDist(dist_uniform(0, 2), "m/s") # made-up
  ),
  inactive = State(
    id = "inactive_on_water",
    energy_cost = VarDist(dist_normal(150, 10), units = "kJ/hour/gram"), # made-up
    time_budget = VarDist(dist_uniform(10, 11), "hours/day"), # roughly based on Lila's summaries
    speed = VarDist(dist_uniform(0, 1), "m/s") # made-up
  ),
  colony = State(
    id = "colony",
    energy_cost = VarDist(dist_normal(33.8, 11.4), units = "kJ/hour/gram"), # vals from excel sheet
    time_budget = VarDist(0, "hours/day"), # no time at colony
    speed = VarDist(0, "m/s") # made-up
  )
)



# Response to drivers  -------------------------------------------------

driver_resp <- list(
  coast = DriverResponse(
    driver_id = "land",
    movement = MoveInfluence(
      prob = VarDist(1),
      fn = function(x, slope = 1/2) exp(-slope * x),
      type = "repulsion"
    )),
  owf = DriverResponse(
    driver_id = "owf_foot",
    movement = MoveInfluence(
      prob = VarDist(dist_beta(2, 8)), # made-up
      fn = function(x, slope = 1/2) exp(-slope * x),
      type = "repulsion"
    ))#,
  # dens = DriverResponse(
  #   driver_id = "dens",
  #   movement = MoveInfluence(
  #     prob = VarDist(1),
  #     type = "attraction"
  #   )),
  # conspec = DriverResponse(
  #   driver_id = "conspec",
  #   movement = MoveInfluence(
  #     prob = VarDist(1),
  #     type = "repulsion"
  #   ))
)



# Specify <Species>  -------------------------------------------------

guill <- Species(
  id = "guill",
  common_name = "guillemot",
  scientific_name = "Uria Aalge",
  pop_size = 15000,
  body_mass_distr = VarDist(dist_uniform(600, 1450), "g"),
  mortality_thresh_distr = VarDist(800, "g"),
  states_profile = states,
  driver_responses = driver_resp
)


# Initiate Agent  -------------------------------------------------
Agent(species = guill, model_config = guill_imb_config)








# FOR REFERENCE: Initiate IBM -------------------------------------------------

guill_drivers <- list(
  drv_land = Driver(
    id = "land",
    type = "habitat",
    descr = "land",
    sf_obj = uk_land |> st_transform(utm30)
  ),
  drv_owfs = Driver(
    id = "owf_foot",
    type = "impact",
    descr = "OWF Footprints",
    sf_obj = owf_foots |> st_transform(utm30)
  ),
  drv_dens = Driver(
    id = "dens",
    type = "habitat",
    descr = "Guillemot spatial distribution",
    stars_obj = stars::st_as_stars(matrix(NA))  # plugin appropriate raster
  ),
  drv_conspec = Driver(
    id = "conspec",
    type = "habitat",
    descr = "conspecific distribution",
    stars_obj = stars::st_as_stars(matrix(NA)) # plugin appropriate raster
  )
)


guill_ibm <- rmr_initiate(
  species = guill,
  drivers = guill_drivers,
  model_config = guill_imb_config
)








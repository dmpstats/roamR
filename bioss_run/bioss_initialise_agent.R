
library(tidyverse)
library(roamR)
library(stars)
library(sf)
library(distributional)
library(CircStats)

library(devtools)
load_all()

# UTM zone 30N
utm30 <- st_crs(32630)


# Input data layers -----------------------------------------------------------------------------------------------

density_map <- readRDS("vignettes/articles/data/GuillemotIsle of May3_iteration1.rds") %>%
  st_as_stars() %>%
  rename(pop_dens = X3)

dens_crop <- st_bbox(c(xmin = -5e5, ymin = -4e6,  xmax = 5e5, ymax = -3.2e6), crs = st_crs(density_map))

density_map <- st_crop(density_map, dens_crop)

competitor_map <- readRDS("vignettes/articles/data/competition_layer_conmap_UK9004171_3.rds") %>%
  st_as_stars() %>%
  rename(compet_dens = layer.1) %>%
  st_crop(dens_crop)

IFD_map <- (density_map + competitor_map) %>%
  rename(combined_dens = pop_dens)

footprints <- st_read("vignettes/articles/data/Synthetic Polygons/") %>%
  st_transform(st_crs(density_map)) %>%
  st_crop(dens_crop)

coastline <- st_read("vignettes/articles/data/coastline/") %>%
  st_transform(st_crs(density_map)) %>%
  st_crop(dens_crop)


ggplot(coastline) +
  viridis::scale_fill_viridis() +
  geom_stars(data = IFD_map, aes(fill = combined_dens), alpha = 0.3) +
  geom_sf() +
  geom_sf(data = footprints, fill = "lightblue")

sst <- stars::read_ncdf("vignettes/articles/data/sst.mnmean.nc") %>%
  filter(time == "2023-12-01")

source("movement_devel/sst-to-aoc.R")


# crop to AOC
sst_aoc <- dens_crop %>%
  st_transform(4326)

sst_crop <- st_crop(sst, sst_aoc) %>%
  st_transform(st_crs(density_map))

plot(sst_crop, axes = T)



# Set up IBM drivers ----------------------------------------------------------------------------------------------



sst_drv <- Driver(
  id = "sst",
  type = "habitat",
  descr = "Sea Surface Temperature",
  stars_obj = sst_crop %>% st_transform(utm30)
)

coast_drv <- Driver(
  id = "land",
  type = "habitat",
  descr = "Coastline",
  sf_obj = coastline %>% st_transform(utm30)
)

owf_drv <- Driver(
  id = "owf_foot",
  type = "impact",
  descr = "OWF footprints",
  sf_obj = footprints %>% st_transform(utm30)
)


guill_drivers <- list(
  sst = sst_drv,
  coast = coast_drv,
  owf = owf_drv
)



# IBM settings -------------------------------------------------

isle_may <- st_sf(
  id = "Isle of May",
  prop = 1,
  geom = st_sfc(st_point(c(-2.5667, 56.1833))),
  crs = 4326
)


# IBM Settings - modify parameter values to your specific case
guill_imb_config <- ModelConfig(
  n_agents = 1,
  ref_sys = utm30,
  aoc_bbx = dens_crop |> st_transform(utm30) |>  st_bbox(), #
  delta_x = 1000,
  delta_y = 1000,
  time_step = "1 day",
  start_date = Sys.Date(),
  end_date = Sys.Date() + 180, # ~6 months
  start_sites = isle_may |> st_transform(utm30)
)




guill_drivers


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
  speed = VarDist(dist_uniform(0, 1), "m/s") # made-up
),
active = State(
  id = "active_on_water",
  energy_cost = VarDist(dist_normal(150, 10), units = "kJ/hour/gram"), # made-up
  time_budget = VarDist(dist_uniform(10, 11), "hours/day"), # roughly based on Lila's summaries
  speed = VarDist(dist_uniform(0, 1), "m/s") # made-up
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
    ))
  #,
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
simBird_1 <- Agent(species = guill, model_config = guill_imb_config)



guill_ibm <- rmr_initiate(
  species = guill,
  drivers = guill_drivers,
  model_config = guill_imb_config
)






# FOR REFERENCE: Initiate IBM -------------------------------------------------
#
# guill_drivers <- list(
#   drv_land = Driver(
#     id = "land",
#     type = "habitat",
#     descr = "land",
#     sf_obj = uk_land |> st_transform(utm30)
#   ),
#   drv_owfs = Driver(
#     id = "owf_foot",
#     type = "impact",
#     descr = "OWF Footprints",
#     sf_obj = owf_foots |> st_transform(utm30)
#   ),
#   drv_dens = Driver(
#     id = "dens",
#     type = "habitat",
#     descr = "Guillemot spatial distribution",
#     stars_obj = stars::st_as_stars(matrix(NA))  # plugin appropriate raster
#   ),
#   drv_conspec = Driver(
#     id = "conspec",
#     type = "habitat",
#     descr = "conspecific distribution",
#     stars_obj = stars::st_as_stars(matrix(NA)) # plugin appropriate raster
#   )
# )
#
#
# guill_ibm <- rmr_initiate(
#   species = guill,
#   drivers = guill_drivers,
#   model_config = guill_imb_config
# )
#
#
#
# current_time <- now()
# tz(current_time) <- "UTC"
#
# my_config <- ModelConfig(n_agents = 1L,
#                          ref_sys = sf::st_crs(32631), #UTM 31N
#                          #ref_sys = sf::st_crs(4326),
#                          aoc_bbx = c(0, 0, 10, 10),
#                          delta_x = 1000, #m
#                          delta_y = 1000,
#                          time_step = "1 day",
#                          start_date = Sys.Date() - 5,
#                          end_date = Sys.Date(),
#                          start_sites = NULL,
#                          end_sites = NULL
# )
#
# density_map <- st_transform(density_map, my_config@ref_sys)
# AoC <- st_transform(AoC, my_config@ref_sys)
#
# my_config@aoc_bbx <- AoC
#
#
# aoc_grid <- sf::st_make_grid(
#   my_config@aoc_bbx,
#   cellsize = c(my_config@delta_x, my_config@delta_y),
#   what = "centers"
# ) %>%
#   st_sf()
#
# aoc_driver <- generate_aoc_driver(grid = aoc_grid, bbox = AoC)
#
#
# # 1. Start by setting the behaviour profile
#
# SST <- 15
# t_dive <- 5
#
# behav <- list(
#   flight = BehaviourSpec(
#     behav = "flying",
#     energy_cost = VarDist(dist_normal(141, 66), units = "kJ/hour/gram"),
#     time_budget = VarDist(dist_uniform(0.056, 0.056), "hours/day"),
#     speed = VarDist(dist_gamma(shape = 8, rate = 1/0.5), "m/s")
#   ),
#   dive = BehaviourSpec(
#     behav = "diving",
#     energy_cost = VarDist(dist_normal(3.71*(3.11*60/t_dive*(1-exp(-5/1.23))), 1.3), units = "kJ/min/gram"), # note units are per minute
#     time_budget = VarDist(dist_uniform(3.11, 3.11), "hours/day"),
#     speed = VarDist(dist_uniform(0, 1), "m/s")
#   ),
#   active = BehaviourSpec(
#     behav = "swimming",
#     energy_cost = VarDist(dist_normal(113-(2.75*SST), 22), units = "kJ/hour/gram"),
#     time_budget = VarDist(dist_uniform(10.5, 10.5), "hours/day"),
#     speed = VarDist(dist_uniform(0, 1), "m/s")
#   ),
#   inactive = BehaviourSpec(
#     behav = "other",
#     energy_cost = VarDist(dist_normal(72.2-(2.75*SST), 22), units = "kJ/hour/gram"),
#     time_budget = VarDist(dist_uniform(10.3, 10.3),"hours/day"),
#     speed = VarDist(dist_uniform(0, 0.1), "m/s")
#   ),
#   colony = BehaviourSpec(
#     behav = "nest_attending",
#     energy_cost = VarDist(dist_normal(33.8, 11.4), units = "kJ/hour/gram"),
#     time_budget = VarDist(dist_uniform(0, 0), "hours/day"),
#     speed = VarDist(dist_uniform(0, 0), "m/s")
#   )
# )
#
#
#
# # 2. the list of impact responses
# imp_resp <- list(
#   owf_footprint = ImpactResponse(
#     impact_id = "owf_foot",
#     displace_prob = VarDist(dist_beta(2, 8)),
#     displace_fn = function(x, y){ x/y},
#     disturb_prob = VarDist(dist_beta(1, 5)),
#     disturb_behav = "flying",
#     # extra hours/day of flight due to impact
#     disturb_extent = VarDist(dist_lognormal(2, 1), "hr/day")
#   )
# )
#
# # 3. `stars` array eith 10 samples of monthly density distribution
# dens <- data.frame(
#   expand.grid(x= 1:5, y = 1:5, month = 3:5, iter = as.integer(1:10)),
#   counts = rlnorm(5*5*3*10)) |>
#   st_as_stars(dims = c("x", "y", "month", "iter"))
#
# # 4. specify <Species>
# guill <- Species(
#   id = "guill",
#   role = "agent",
#   common_name = "guillemot",
#   scientific_name = "Uria Aalge",
#   body_mass_distr = VarDist(dist_uniform(850, 1130), "g"),
#   mortality_thresh_distr = VarDist(dist_normal(500, 20), "g"),
#   spatial_distr = dens,
#   behaviour_profile = behav,
#   impact_responses = imp_resp
# )
#
#
#
# lila_dat <- read_csv("data/ActivityBudget.csv")
#
# lila_mat <- lila_dat %>%
#   group_by(Colony, TDR_ID, Date, Behaviour) %>%
#   summarise(sum(Hours))

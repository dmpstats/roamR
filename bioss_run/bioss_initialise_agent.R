
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

# coastline <- st_read("vignettes/articles/data/coastline/") %>%
#   st_transform(st_crs(density_map)) %>%
#   st_crop(dens_crop) %>%
#   st_combine()


coastline <- ggplot2::map_data("world", region = "UK") |>
  st_as_sf(coords = c("long", "lat"),  crs = 4326) |>
  group_split(group) |>
  purrr::map(\(x){
    st_combine(x) |>
      st_cast("POLYGON")
  } ) |>
  purrr::list_c() |>
  st_combine() %>%
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

# due to such low res, we need to interpolate - a fancy model would be best, here just mean
sst_crop <- st_crop(sst, sst_aoc)
sst_crop$sst <- ifelse(is.na(sst_crop$sst), mean(sst_crop$sst, na.rm = T), sst_crop$sst)

sst_crop <- sst_crop %>%
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
  start_date = date("2025-07-01"),
  end_date = date("2025-07-01") + 270, # ~9 months
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

guill_ibm <- rmr_initiate(
  species = guill,
  drivers = guill_drivers,
  model_config = guill_imb_config
)




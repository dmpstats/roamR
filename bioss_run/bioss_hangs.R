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

footprints <- st_read("vignettes/articles/data/Synthetic Polygons/") %>%
  st_transform(st_crs(density_map)) %>%
  st_crop(dens_crop)

coastline <- st_read("vignettes/articles/data/coastline/") %>%
  st_transform(st_crs(density_map)) %>%
  st_crop(dens_crop)

# deal with weird SST

sst <- stars::read_ncdf("vignettes/articles/data/sst.mnmean.nc") %>%
  filter(time == "2023-12-01")

source("movement_devel/sst-to-aoc.R")

sst_aoc <- dens_crop %>%
  st_transform(4326)

sst_crop <- st_crop(sst, sst_aoc) %>%
  st_transform(st_crs(density_map))


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



# Set up drivers --------------------------------------------------------------------------------------------------


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
      type = "repulsion"))
)



# Specify <Species>  -------------------------------------------------

guill <- Species(
  id = "guill",
  common_name = "guillemot",
  scientific_name = "Uria Aalge",
  pop_size = 15000,
  body_mass_distr = VarDist(dist_uniform(600, 1450), "g"),
  mortality_thresh_distr = VarDist(800, "g"),
  states_profile = list(),
  driver_responses = driver_resp
)


# Initiate IBM -----------------------------------------------------------------------------------------------------

# = this works

guill_drivers <- list(
  sst = sst_drv,
  owf = owf_drv
)



guill_ibm <- rmr_initiate(
  species = guill,
  drivers = guill_drivers,
  model_config = guill_imb_config
)


# = this hangs

guill_drivers <- list(
  sst = sst_drv,
  coast = coast_drv,
  owf = owf_drv
)



guill_ibm <- rmr_initiate(
  species = guill,
  drivers = guill_drivers,
  model_config = guill_imb_config
)


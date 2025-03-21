library(tidyverse)
library(devtools)
library(sf)
library(stars)
load_all()

# UTM zone 30N
utm30 <- st_crs(32630)

source("movement_devel/sample_cell.R")
source("movement_devel/calc_day_costs.R")
source("movement_devel/sample_cell.R")
source("movement_devel/run_sim.R")

guill_imb_config <- readRDS("movement_devel/guill_ibm_config.rds")
guill <- readRDS("movement_devel/guill_species.rds")


density_map <- readRDS("vignettes/articles/data/GuillemotIsle of May3_iteration1.rds") %>%
  st_as_stars() %>%
  rename(pop_dens = X3)

dens_crop <- st_bbox(c(xmin = -5e5, ymin = -4e6,  xmax = 5e5, ymax = -3.2e6), crs = st_crs(density_map))

density_map <- st_crop(density_map, dens_crop)

template_rast <- st_as_stars(guill_imb_config@aoc_bbx, nx = dim(density_map)[1], ny = dim(density_map)[2], values = NA_real_)

density_map <- st_warp(density_map, template_rast)



# Input data layers -----------------------------------------------------------------------------------------------


  current_time <- guill_imb_config@start_date + days(1)
  guill_imb_config@end_date <- guill_imb_config@start_date + days(100)

  step_duration <- days(1)

  agent_list <- list()

  set.seed(4958)

  for(i in 1:4){

  agent_list[[i]] <- Agent(species = guill, model_config = guill_imb_config)

  agent_list[[i]]@history$energy_expenditure <- agent_list[[i]]@history$energy_expenditure %>%
    units::drop_units() %>%
    units::set_units(., "kJ")

  agent_list[[i]]@history <- agent_list[[i]]@history %>%
    st_set_crs(guill_imb_config@ref_sys)

  } # eol i


  # future::plan(future::multisession, workers = 4)
  #
  #
  #   test <- agent_list %>%
  #     furrr::future_map(.f = \(x) run_sim(in_agent = x, in_species = guill, in_ibm_config = guill_imb_config,
  #                                         in_density = density_map), .progress = T)



   test <- lapply(agent_list, \(x) run_sim(in_agent = x, in_species = guill, in_ibm_config = guill_imb_config,
            in_density = density_map))

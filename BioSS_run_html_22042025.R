## -----------------------------------------------------------------------------
#setwd("C://Users//ND44442//OneDrive - The James Hutton Institute//Documents//owf//")
args <- commandArgs(trailingOnly = TRUE) #returns a list of arguments passed to R via the command line
                                         #trailingOnly filters the list to return only the arguments that come after the script name in the .sh file
task_id <- ifelse(length(args) > 0, as.integer(args[1]), 1) #if nothing is passed, task_id defaults to 1

library(tidyverse)
library(units)
#devtools::install_github("dmpstats/roamR@BioSS-sims", auth_token = "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")
library(roamR)
library(stars)
library(sf)
library(distributional)
library(spaths)

# UTM zone 30N
utm30 <- st_crs(32630)


# AoC in UTM. Exact figures come from a previous lat/lon example
AoC <- st_bbox(c(xmin = 178831, ymin = 5906535,  xmax = 1174762, ymax = 6783609), crs = st_crs(utm30))


# location of colony - start/finish locations

isle_may <- st_sf(
  id = "Isle of May",
  prop = 1,
  geom = st_sfc(st_point(c(-2.5667, 56.1833))),
  crs = 4326
)



# IBM Settings - assume fixed for these simulations

guill_imb_config <- ModelConfig(
  n_agents = 1,
  ref_sys = utm30,
  aoc_bbx = AoC, #
  delta_x = 1000,
  delta_y = 1000,
  time_step = "1 day",
  start_date = date("2025-07-01"),
  end_date = date("2025-07-01") + 270, # ~9 months
  start_sites = isle_may |> st_transform(utm30),
  end_sites = isle_may |> st_transform(utm30)
)


# Input data layers -----------------------------------------------------------------------------------------------
# datacubes: density, lat/lon/, month
spec_map <- readRDS("bioss_run/data/bioss_spec_map.rds")

conspec_map <- readRDS("bioss_run/data/bioss_conspec_map.rds")

spec_imp_map <- readRDS("bioss_run/data/bioss_spec_imp_map.rds")

conspec_imp_map <- readRDS("bioss_run/data/bioss_conspec_imp_map.rds")

energy_imp_map <- readRDS("bioss_run/data/bioss_energy_imp_map.rds")

sst <- readRDS("bioss_run/data/bioss_sst_stars.rds")


## -----------------------------------------------------------------------------
# Set up IBM drivers ----------------------------------------------------------------------------------------------

sst_drv <- Driver(
  id = "sst",
  type = "habitat",
  descr = "Sea Surface Temperature",
  stars_obj = sst
)


dens_drv <- Driver(
  id = "dens",
  type = "impact",
  descr = "species dens map",
  stars_obj = spec_map
)

dens_imp_drv <- Driver(
  id = "dens_imp",
  type = "impact",
  descr = "species redist map",
  stars_obj = spec_imp_map
)

consp_drv <- Driver(
  id = "consp_dens",
  type = "impact",
  descr = "conspecific dens map",
  stars_obj = conspec_map
)

consp_imp_drv <- Driver(
  id = "consp_imp_dens",
  type = "impact",
  descr = "conspecific redist map",
  stars_obj = conspec_imp_map
)


energy_imp_drv <- Driver(
  id = "energy_imp",
  type = "impact",
  descr = "energy impact map",
  stars_obj = energy_imp_map
)


# store as list for initialisation
guill_drivers <- list(
  sst = sst_drv,
  dens = dens_drv,
  consp = consp_drv,
  dens_imp = dens_imp_drv,
  consp_imp = consp_imp_drv,
  energy_imp = energy_imp_drv
)


## -----------------------------------------------------------------------------
# States specification -------------------------------------------------

states <- list(
  flight = State(
    id = "flight",
    energy_cost = VarDist(dist_normal(507.6, 237.6), units = "kJ/hour"), # vals from excel sheet. Normal may not be adequate (some good chance of <zero) - there is a catch for this
    time_budget = VarDist(dist_degenerate(0.056), "hours/day"), # Based on Lila's summary data
    speed = VarDist(dist_uniform(10, 20), "m/s") # placeholder, not very relevant
  ),
  dive = State(
    id = "diving",
    energy_cost = VarDist(dist_normal(3.71, 1.3), units = NULL), # from spreadsheet
    time_budget = VarDist(dist_degenerate(3.11), "hours/day"), # roughly based on Lila's summaries
    speed = VarDist(dist_uniform(0, 1), "m/s") # placeholder, not very relevant
  ),
  active = State(
    id = "active_on_water",
    energy_cost = VarDist(dist_normal(113, 22), units = NULL), # from spreadsheet
    time_budget = VarDist(dist_degenerate(10.5), "hours/day"), # Based on Lila's summaries
    speed = VarDist(dist_uniform(0, 1), "m/s") # placeholder, not very relevant
  ),
  inactive = State(
    id = "inactive_on_water",
    energy_cost = VarDist(dist_normal(72.2, 22), units = NULL), # from spreadsheet
    time_budget = VarDist(dist_degenerate(10.3), "hours/day"), # Based on Lila's summaries
    speed = VarDist(dist_uniform(0, 1), "m/s") # placeholder, not very relevant
  ),
  colony = State(
    id = "colony",
    energy_cost = VarDist(dist_normal(33.8, 11.4), units = "kJ/hour"), # from spreadsheet
    time_budget = VarDist(dist_degenerate(0), "hours/day"), # no time at colony
    speed = VarDist(0, "m/s") # placeholder, not very relevant
  )
)


## -----------------------------------------------------------------------------
# Specify <Species>  -------------------------------------------------

guill <- Species(
  id = "guill",
  common_name = "guillemot",
  scientific_name = "Uria Aalge",
  pop_size = 15000,
  body_mass_distr = VarDist(dist_normal(mean = 929, sd = 56), "g"), # mean from kate CEH doc - sd digitising Francis' plot
  mortality_thresh_distr = VarDist(800, "g"), # not used specifically - dealt with in post-processing (e.g. <60% of starting bodymass)
  states_profile = states,
  driver_responses = list()
)


# probability an agent responds to OWF - a Bioss sensitivity parameter
# Spreadsheet sets this to 67%, with (95% ?) CI of 53-77. NB implies a SD of abount 6.1 if normal approx
# use mean for example run

p_imp_response <- 0.67


## -----------------------------------------------------------------------------
# Initiate Agent  -------------------------------------------------

guill_ibm <- rmr_initiate(
  species = guill,
  drivers = guill_drivers,
  model_config = guill_imb_config
)


## -----------------------------------------------------------------------------
agent_list <- list()
  n_agents <- 400

  set.seed(4958)

  for(i in 1:n_agents){

  agent_list[[i]] <- Agent(species = guill, model_config = guill_imb_config)

  agent_list[[i]]@history <- agent_list[[i]]@history |>
    st_set_crs(guill_imb_config@ref_sys)

  agent_list[[i]]@properties@move_influences <- list(owf_imp = rbinom(1, 1, p_imp_response))

  }


## -----------------------------------------------------------------------------
future::plan(future::multisession, workers = 6)

set.seed(34958) # important this be set to allow paired runs with the impacted scenarios

options(error = recover)
options(error = NULL)

.Random.seed <- readRDS("sim_error_seed.rds")

#debug(path_calc)

sapply(185, \(x) {print(x); #saveRDS(.Random.seed, "sim_seed.rds")
            bioss_run_sim(
            in_agent = agent_list[[x]], in_ibm = guill_ibm,
            in_species = guill,
            in_ibm_config = guill_imb_config,
            mean_intake = 543,
            impact = F)})


system.time({

  sequent_imp_runs <- furrr::future_map(agent_list, \(x) bioss_run_sim(
    in_agent = x, in_ibm = guill_ibm,
    in_species = guill,
    in_ibm_config = guill_imb_config,
    mean_intake = 543,
    impact = F),
    .progress = T, .options = furrr::furrr_options(seed = T))

})

## -----------------------------------------------------------------------------
slotNames(sequent_runs[[1]])


## -----------------------------------------------------------------------------
hist_obj <- lapply(sequent_runs, \(x) x@history)

names(hist_obj) <- paste0("agent_", 1:length(hist_obj))

hist_obj <- bind_rows(hist_obj, .id = "agent_id") |>
  mutate(impact = F)

## -----------------------------------------------------------------------------
future::plan(future::multisession, workers = 4)

set.seed(34958) # seed matches previous run for pairing

system.time({

sequent_imp_runs <- furrr::future_map(agent_list, \(x) bioss_run_sim(
            in_agent = x, in_ibm = guill_ibm,
            in_species = guill,
            in_ibm_config = guill_imb_config,
            mean_intake = 543,
            impact = T), # OWF information is observed
            .progress = T, .options = furrr::furrr_options(seed = T))

})


## -----------------------------------------------------------------------------
imp_hist_obj <- lapply(sequent_imp_runs, \(x) x@history)

names(imp_hist_obj) <- paste0("agent_", 1:length(imp_hist_obj))

imp_hist_obj <- bind_rows(imp_hist_obj, .id = "agent_id") |>
  mutate(impact = T)


comp_df <- bind_rows(hist_obj, imp_hist_obj)



saveRDS(imp_hist_obj, paste0("Output/", task_id, "_imp_hist_obj.rds"))
saveRDS(hist_obj, paste0("Output/", task_id, "_hist_obj.rds"))


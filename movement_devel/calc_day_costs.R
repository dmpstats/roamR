# -----------------------------------------------------------------------------------------------------------------
#' Calculate day-level energy costs as a function of:
#'  - activity budget
#'  - SST
#'  - condition
#'  - time distribution

library(devtools)
library(tidyverse)
library(sf)

load_all()

# UTM zone 30N
utm30 <- st_crs(32630)

source("movement_devel/sample_cell.R")
guill_imb_config <- readRDS("movement_devel/guill_ibm_config.rds")
guill <- readRDS("movement_devel/guill_species.rds")


simBird <- Agent(species = guill, model_config = guill_imb_config)

names(simBird@condition@states_budget)
names(guill@states_profile)

source("movement_devel/sample_dive_time.R")



# Bespoke cost functions ------------------------------------------------------------------------------------------
#' A series of functions to calculate costs - these will be coded for generality in roamR proper


dive_cost_fn <- function(t_dive, species){

  x <- units::drop_units(t_dive)

  rand_par <- generate(species@states_profile$dive@energy_cost, 1)

  rand_par*sum(1-exp(-x/1.23))/sum(x)*60 %>%
    units::set_units(., kJ/hr)

  }


active_water_cost_fn <- function(sst, species){

  rand_par <- generate(species@states_profile$active@energy_cost, 1) %>%
    units::drop_units()

  (rand_par-(2.75*sst)) %>%
    units::set_units(., kJ/hr)

}


inactive_water_cost_fn <- function(sst, species){

  rand_par <- generate(species@states_profile$inactive@energy_cost, 1) %>%
    units::drop_units()

  (rand_par-(2.75*sst)) %>%
    units::set_units(., kJ/hr)

}

flight_cost_fn <- function(species){

  generate(species@states_profile$flight@energy_cost, 1)

}

colony_cost_fn <- function(species){

  generate(species@states_profile$colony@energy_cost, 1)

}



# Calc costs ------------------------------------------------------------------------------------------------------
#' A function of the activity and the amount of time spent in the data (and potentially SST)


calc_day_cost <- function(in_agent, in_species, in_ibm) {

  sst <- st_extract(in_ibm@drivers$sst@stars_obj, st_sfc(in_agent@condition@location, crs = utm30))

  costs <- list(flight = flight_cost_fn(species = in_species),
                dive = dive_cost_fn(species = in_species, t_dive = 1.05),
                active = active_water_cost_fn(sst = sst, species = in_species),
                inactive = inactive_water_cost_fn(sst = sst, species = in_species),
                colony = colony_cost_fn(species = in_species)) %>%
    as.data.frame()


  in_agent@condition@states_budget %>%
    as.data.frame() %>%
    units::drop_units() %>%
    pivot_longer(names_to = "state", values_to = "prop", everything()) %>%
    mutate(time = prop*24,
           time = units::set_units(time, h)) %>%
    left_join(costs)


}







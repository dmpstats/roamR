# -----------------------------------------------------------------------------------------------------------------
#' Calculate day-level energy costs as a function of:
#'  - activity budget
#'  - SST
#'  - condition
#'  - time distribution

library(devtools)
library(tidyverse)
library(sf)
library(stars)

load_all()

# UTM zone 30N
utm30 <- st_crs(32630)

source("movement_devel/sample_cell.R")
guill_imb_config <- readRDS("movement_devel/guill_ibm_config.rds")
guill_ibm <- readRDS("movement_devel/guill_ibm.rds")
guill <- readRDS("movement_devel/guill_species.rds")
in_lat <- st_transform(guill_imb_config@start_sites, crs = 4326)$geom[[1]][2]


simBird <- Agent(species = guill, model_config = guill_imb_config)

names(simBird@condition@states_budget)
names(guill@states_profile)

source("movement_devel/sample_dive_time.R")



# Bespoke cost functions ------------------------------------------------------------------------------------------
#' A series of functions to calculate costs - these will be coded for generality in roamR proper


dive_cost_fn <- function(t_dive, species){

  x <- units::drop_units(t_dive)

  rand_par <- generate(species@states_profile$dive@energy_cost, 1) %>%
    units::drop_units()

  max((rand_par*sum(1-exp(-x/1.23))/sum(x)*60), 0) %>%
    units::set_units(., kJ/hr)

  }


active_water_cost_fn <- function(sst, species){

  rand_par <- generate(species@states_profile$active@energy_cost, 1) %>%
    units::drop_units()

  max((rand_par-(2.75*sst)), 0) %>%
    units::set_units(., kJ/hr)

}


inactive_water_cost_fn <- function(sst, species){

  rand_par <- generate(species@states_profile$inactive@energy_cost, 1) %>%
    units::drop_units()

  max((rand_par-(2.75*sst)), 0) %>%
    units::set_units(., kJ/hr)

}

flight_cost_fn <- function(species){

  max(generate(species@states_profile$flight@energy_cost, 1), units::set_units(0, "kJ/hr"))

}

colony_cost_fn <- function(species){

  max(generate(species@states_profile$colony@energy_cost, 1), units::set_units(0, "kJ/hr"))

}



# Calc costs ------------------------------------------------------------------------------------------------------
#' A function of the activity and the amount of time spent in the data (and potentially SST)
#' Assume 593 kJ per hour of feeding on average

calc_day_cost <- function(in_agent, in_species, in_ibm, intake) {

  sst <- st_extract(in_ibm@drivers$sst@stars_obj, st_sfc(in_agent@condition@location, crs = in_ibm@model_config@ref_sys))$sst

  costs <- list(flight = -flight_cost_fn(species = in_species),
                dive = -dive_cost_fn(species = in_species, t_dive = units::set_units(1.05, "min")) + intake,
                active = -active_water_cost_fn(sst = sst, species = in_species),
                inactive = -inactive_water_cost_fn(sst = sst, species = in_species),
                colony = -colony_cost_fn(species = in_species)) %>%
    as.data.frame() %>%
    pivot_longer(names_to = "state", values_to = "unit_cost", everything())


  in_agent@condition@states_budget %>%
    as.data.frame() %>%
    units::drop_units() %>%
    pivot_longer(names_to = "state", values_to = "prop", everything()) %>%
    mutate(time = prop*24,
           time = units::set_units(time, h)) %>%
    left_join(costs, by = "state") %>%
    mutate(day_cost = time*unit_cost)


}

test <- calc_day_cost(in_agent = simBird, in_species = guill, in_ibm = guill_ibm, intake = units::set_units(593, "kJ/h"))
test

sample_vect <- numeric(270)

for(i in 1:270){test <- calc_day_cost(in_agent = simBird, in_species = guill, in_ibm = guill_ibm, intake = units::set_units(593, "kJ/h")); sample_vect[i] <- sum(test$day_cost)}
plot(cumsum(sample_vect*0.072))



# LP for state proportions ----------------------------------------------------------------------------------------


night_length <- 24 - geosphere::daylength(lat = in_lat, doy = yday(simBird@condition@timestamp))


lp_coef <- test$unit_cost
lp_constr <- matrix(c(rep(1, 5),
                      c(1, 0, 0, 0, 0),
                      c(0, 1, 0, 0, 0),
                      c(0, 0, 1, 0, 0),
                      c(0, 0, 0, 1, 0),
                      c(0, 0, 0, 0, 1)), ncol = 5)
lp_b <- c(538, 0, 0, 0, 0, 0)
lp_dir <- c(">=", ">=", ">=", ">=", ">=", "=")

lp_solve <- lpSolve::lp("max", lp_coef, lp_constr, lp_dir, lp_b)
lp_solve$solution


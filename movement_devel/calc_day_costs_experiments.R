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


# scratch ---------------------------------------------------------------------------------------------------------


#
#
# dive_cost_fn <- function(t_dive, species){
#
#   x <- units::drop_units(t_dive)
#
#   rand_par <- 3.7
#
#   max((rand_par*sum(1-exp(-x/1.23))/sum(x)*60), 0) %>%
#     units::set_units(., kJ/hr)
#
# }
#
#
# active_water_cost_fn <- function(sst, species){
#
#   rand_par <- 113
#
#   max((rand_par-(2.75*sst)), 0) %>%
#     units::set_units(., kJ/hr)
#
# }
#
#
# inactive_water_cost_fn <- function(sst, species){
#
#   rand_par <- 72
#
#   max((rand_par-(2.75*sst)), 0) %>%
#     units::set_units(., kJ/hr)
#
# }
#
# flight_cost_fn <- function(species){
#
#   units::set_units(508, "kJ/hr")
#
# }
#
# colony_cost_fn <- function(species){
#
#  units::set_units(34, "kJ/hr")
#
# }
#


# scratch ---------------------------------------------------------------------------------------------------------




dive_cost_fn <- function(t_dive, species){

  x <- units::drop_units(t_dive)

  rand_par <- generate(species@states_profile$dive@energy_cost, 1) %>%
    units::drop_units()

  (rand_par*sum(1-exp(-x/1.23))/sum(x)*60) %>%
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
#' Assume 593 kJ per hour of feeding on average


calc_day_cost <- function(in_agent, in_species, in_ibm, intake, sst) {

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
    mutate(time = units::set_units(prop*24, "h")) %>%
    left_join(costs, by = "state") %>%
    mutate(day_cost = time * unit_cost)

}




set.seed(934875)

test_bird <- simBird

mean_sst <- st_extract(guill_ibm@drivers$sst@stars_obj, st_sfc(test_bird@condition@location, crs = guill_ibm@model_config@ref_sys))$sst
test <- calc_day_cost(in_agent = test_bird, in_species = guill, in_ibm = guill_ibm, sst = mean_sst, intake = units::set_units(589.5, "kJ/h"))
test
sum(test$day_cost)




sample_vect <- numeric(270)

for(i in 1:270){test <- calc_day_cost(in_agent = test_bird, in_species = guill,
                                      in_ibm = guill_ibm, sst = mean_sst, intake = units::set_units(589.5, "kJ/h"))
                sample_vect[i] <- sum(test$day_cost)
                nudge_states <- state_balance(in_states = test[1:4,])
                test_bird@condition@states_budget[1:4] <- nudge_states
                # cat(i, sample_vect[i], "\n")
                }

plot(cumsum(sample_vect*0.072))


# Rebalance states ------------------------------------------------------------------------------------------------

# 4.85
# geometry approach

state_balance <- function(in_states, night_proportion = 0.3, feed_state = 2, rest_state = 4,
                          energy_target = units::set_units(1.14, "kJ/h")){

  out_states <- in_states$prop

  net <- sum(in_states$prop * in_states$unit_cost)

  feed_energy <- in_states$prop[feed_state] * in_states$unit_cost[feed_state]
  target_energy <- in_states$prop[feed_state] * in_states$unit_cost[feed_state] - net + energy_target

  feed_mult <- target_energy/feed_energy

  # lock in feed state: has prop of base and height of in_state i.e. change in average energy  is bu probability change
  out_states[feed_state] <- in_states$prop[feed_state]*feed_mult

  # need to change other states base proportionately - squeeze targets up/down to maintain area/energy
  non_feed_mult <- (1-out_states[feed_state])/sum(in_states$prop[-feed_state])

  out_states[-feed_state] <- in_states$prop[-feed_state]*non_feed_mult

  if(out_states[rest_state] < night_proportion){
    out_states[rest_state] <- night_proportion
    drop_states <- c(feed_state, rest_state)
    non_feed_mult <- (1-sum(out_states[drop_states]))/sum(in_states$prop[-drop_states])
    out_states[-drop_states] <- in_states$prop[-drop_states]*non_feed_mult
  }

  out_states

}


state_balance <- function(in_states, energy_target = units::set_units(0, "kJ/h")){

  net <- sum(in_states$prop * in_states$unit_cost)
  rebal_target <- (energy_target - net)

  target_costs <-in_states$unit_cost + rebal_target

  ratio_vect <- target_costs/in_states$unit_cost
  out_states <- in_states$prop * ratio_vect
  sum_states <- sum(out_states)

  cat("Target", energy_target, "out ", out_states %*% in_states$unit_cost, "\n")

  out_states <- out_states/sum_states

  cat("Vs", out_states %*% in_states$unit_cost, "\n")

  out_states

}



state_balance <- function(in_states, feed_state = 2, energy_target = units::set_units(0, "kJ/h")){

  net <- sum(in_states$prop * in_states$unit_cost)
  rebal_target <- (energy_target - net)

  target_costs <-in_states$unit_cost + rebal_target

  ratio_vect <- target_costs/in_states$unit_cost
  out_states <- in_states$prop * ratio_vect
  sum_states <- sum(out_states)

  cat("Target", energy_target, "out ", out_states %*% in_states$unit_cost, "\n")

  out_states

}



state_balance <- function(in_states, energy_target = 1.14, pos_states = 2){
  not_pos <- c(1:nrow(in_states))[-pos_states]
  net <- sum(in_states$prop * in_states$unit_cost) %>% units::drop_units()
  rebal_target <- (energy_target - net)

  prop_pos <- sum(in_states$prop[pos_states])
  prop_neg <- sum(in_states$prop[not_pos])

  pos_costs <- sum(in_states$prop[pos_states] * in_states$unit_cost[pos_states]) %>% units::drop_units()
  neg_costs <- sum(in_states$prop[not_pos] * in_states$unit_cost[not_pos]) %>% units::drop_units()

  A <- (rebal_target - pos_costs)/(neg_costs - pos_costs)

  out_states <- in_states$prop
  out_states[pos_states] <- out_states[pos_states]*(1-A)
  out_states[not_pos] <- out_states[not_pos]*A
  out_states/sum(out_states)

}


state_balance <- function(in_states, energy_target, pos_states = 2){
  pos_states <- 2
  in_states <- test[1:4,]
  energy_target <- units::set_units(1.14, "kJ/h")


  not_pos <- c(1:nrow(in_states))[-pos_states]
  net <- sum(in_states$unit_cost)
  rebal_target <- (energy_target - net)  %>% units::drop_units()

  prop_pos <- sum(in_states$prop[pos_states])
  prop_neg <- sum(in_states$prop[not_pos])

  pos_costs <- sum(in_states$prop[pos_states] * in_states$unit_cost[pos_states]) %>% units::drop_units()
  neg_costs <- sum(in_states$prop[not_pos] * in_states$unit_cost[not_pos]) %>% units::drop_units()

  A <- (rebal_target - pos_costs)/(neg_costs - pos_costs)

  out_states <- in_states$prop
  out_states[pos_states] <- out_states[pos_states]*(1-A)
  out_states[not_pos] <- out_states[not_pos]*A
  #out_states/sum(out_states)
  out_states
}


# LP for state proportions ----------------------------------------------------------------------------------------

net <- sum(test$day_cost)
net

night_length <- 24 - geosphere::daylength(lat = in_lat, doy = yday(simBird@condition@timestamp))


lp_coef <- test$unit_cost
lp_constr <- matrix(c(rep(1, 5),
                      c(1, 0, 0, 0, 0),
                      c(0, 1, 0, 0, 0),
                      c(0, 0, 1, 0, 0),
                      c(0, 0, 0, 1, 0),
                      c(0, 0, 0, 1, 0),
                      c(0, 0, 0, 0, 1)), ncol = 5, byrow = T)
lp_b <- c(1, 0, 0, 0, 0, night_length/24, 0)
lp_dir <- c("<=", ">=", ">=", ">=", ">=", ">=", "=")

lp_solve <- lpSolve::lp("max", lp_coef, lp_constr, lp_dir, lp_b)
lp_solve
lp_solve$solution
lp_solve$solution %*% lp_coef
(lp_solve$solution %*% lp_coef)*0.072

test$time %*% test$unit_cost

sum(test$day_cost)




# -----------------------------------------------------------------------------------------------------------------


lp_coef <- c(units::drop_units(test$unit_cost), -1.4)
lp_constr <- matrix(c(c(rep(1, 5),0),
                      # c(1, 0, 0, 0, 0),
                      c(0, 0, 0, 0, 0, 1),
                      c(0, 0, 0, 1, 0, 0),
                      c(0, 0, 0, 0, 1, 0)), ncol = 6, byrow = T)
lp_b <- c(1, 1, night_length/24, 0)
lp_dir <- c("=", "=", ">=", "=")

lp_solve <- lpSolve::lp("max", lp_coef, lp_constr, lp_dir, lp_b)
lp_solve
lp_solve$solution
lp_solve$solution %*% lp_coef
(lp_solve$solution %*% lp_coef)*0.072

test$time %*% test$unit_cost

sum(test$day_cost)


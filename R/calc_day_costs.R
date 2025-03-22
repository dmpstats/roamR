# -----------------------------------------------------------------------------------------------------------------


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



# Rebalance states ------------------------------------------------------------------------------------------------

state_balance <- function(in_states, night_proportion, feed_state = 2, rest_state = 4, energy_target = units::set_units(1.14, "kJ/h")){

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


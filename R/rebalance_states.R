#' Rebalance activity states
#' For BioSS analyses
#'
#' @param in_states A data frame of states and their proportions and costs
#' @param night_proportion Proportion of day that is night (rest state cannot fall below this)
#' @param feed_state Index of feeding/energy in state
#' @param rest_state Index of resting state (where night constraint applies)
#' @param energy_target Target energy (kJ/h) - must have units
#'
#' @returns A vector of proportions
#' @export
#'
#' @examples TBD
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

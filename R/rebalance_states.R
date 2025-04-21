#' Rebalance activity states
#' For BioSS analyses
#'
#' @param in_states A data frame of states and their proportions and costs
#' @param night_proportion Proportion of day that is night (rest state cannot fall below this)
#' @param feed_state Index of feeding/energy in state
#' @param rest_state Index of resting state (where night constraint applies)
#' @param energy_target cumulative energy target
#'
#' @returns A vector of proportions
#' @export
#'
#' @examples TBD
state_balance <- function(in_states, night_proportion, feed_state = 2, rest_state = 4, max_nudge = 0.1, current_e, exp_e = 422, energy_target = 0){

  current_e <- units::drop_units(current_e)

  out_states <- in_states$prop
  feed_prop <- in_states$prop[feed_state]

  # upper_feed_bound <- min(1 - night_proportion, (1 + max_nudge) * feed_prop)
  # lower_feed_bound <- max(0, (1 - max_nudge) * feed_prop)

  # upper_feed_bound <- min(1 - night_proportion, feed_prop + max_nudge)
  # lower_feed_bound <- max(0, feed_prop - max_nudge)

  upper_feed_bound <- (1 - night_proportion)
  lower_feed_bound <- 0


  net_target_e <- energy_target - current_e
  ratio_e <- net_target_e / exp_e

  #move_p <- feed_prop + tanh(ratio_e) * max_nudge
  move_p <- ratio_e/24
  move_p <- min(move_p, upper_feed_bound)
  move_p <- max(move_p, lower_feed_bound)

  out_states[feed_state] <- move_p

  non_feed_mult <- (1 - out_states[feed_state]) / sum(in_states$prop[-feed_state])
  out_states[-feed_state] <- in_states$prop[-feed_state] * non_feed_mult

  if(out_states[rest_state] < night_proportion){

    out_states[rest_state] <- night_proportion

    drop_states <- c(feed_state, rest_state)

    non_feed_mult <- (1 - sum(out_states[drop_states])) / sum(in_states$prop[-drop_states])

    out_states[-drop_states] <- in_states$prop[-drop_states] * non_feed_mult
  }

  out_states

}

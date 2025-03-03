# basic movement function
# takes current location [x, y] + realised parameters for turning and stepping
# returns new location [x, y]
# NB: time/units/projection

move_agent <- function(current_pos, direct_m, direct_v, state_speed){

  new_direction <- rwrpnorm(1, mu = direct_m, sd = direct_v)
  # new_step <- rgamma(1, shape = step_shape, scale = step_scale)

  add_x <- state_speed*cos(new_direction)
  add_y <- state_speed*sin(new_direction)

  out_pos <- current_pos + c(add_x, add_y)
  st_crs(out_pos) <- st_crs(current_pos)

  out_pos

}

# Calculate the direction between two points (first and finish)
# Uses the standard radial angle system throughout - 0 being horizontal right, counter clockwise angles in radians

calc_direction <- function(start, finish){

  diff <- unlist(finish) - unlist(start) # much faster if reduced to vectors cf spatial

  atan2(diff[2], diff[1])

}


quick_extract_pt <- function(in_rast, in_point){
  loc <- terra::cellFromXY(in_rast, matrix(unlist(in_point), ncol = 2))
  terra::extract(in_rast, loc)
}


simBird <- Agent(species = guill, model_config = guill_imb_config)

simBird@history <- simBird@history %>%
  st_set_crs(guill_imb_config@ref_sys)


set.seed(7657)
nsteps <- 100
sim_day <- 1
dive_duration <- runif(1, 1, 2)
start_state <- 1
init_dir <- 0
var_dir <- 1


state_labels <- names(simBird@condition@states_budget)
state_distrib <- unlist(simBird@condition@states_budget)
current_state <- sample(state_labels, 1, prob = state_distrib)
n_states <- length(state_labels)
state_ind <- 1:n_states
ind_mat <- diag(n_states)

trans_mat <- matrix(rep(state_distrib, length(state_distrib)), nrow = length(state_distrib), byrow = T)

# trans_row_sum <- apply(trans_mat, 1, sum)
# trans_mat <- trans_mat/trans_row_sum

current_pos <- simBird@history$geometry
travel_dir <- init_dir
current_state <- sample(state_ind, 1, prob = state_distrib)


#  names(step_hist) <- c("x", "y", "current_state", "energy_out", "sim_time", "sim_day", "energy_intake")

system.time({

  current_time <- guill_imb_config@start_date + days(1)
  step_duration <- hours(1)


  # daylight_refs <- suncalc::getSunlightTimes(lat = simBird@history$geometry[1], lon = ref_loc[2], date = as_date(current_time))

  while(current_time <= guill_imb_config@end_date){

    # daytime <- current_time %within% interval(daylight_refs$sunrise, daylight_refs$sunset)

    # if(daytime) {

      current_speed <- unlist(simBird@properties@speeds[current_state])

      next_pos <- move_agent(current_pos, travel_dir, var_dir, current_speed)

      current_dir <- calc_direction(current_pos, next_pos)

      #dir_modifiers <- quick_extract_pt(test, next_pos)

      dir_modifiers <- st_extract(guill_ibm@drivers$owf@stars_obj, at = current_pos)

      # dir_modifiers <- quick_extract_pt(repel_array, next_pos)

      # current_state <- sample(1:5, 1, prob = trans_mat[current_state, ])

      # current_expenditure <- ind_mat[current_state, ] %*% state_expenditure

      pars <- c(1, dir_modifiers$slope)
      dirs <- c(current_dir, dir_modifiers$aspect)

      y_tan <- sin(dirs) %*% pars
      x_tan <- cos(dirs) %*% pars

      travel_dir <- atan2(y_tan, x_tan)

      current_pos <- next_pos

    # # } else {
    #
    #   current_state <- 1
    #
    #   current_expenditure <- ind_mat[current_state, ] %*% state_expenditure
    #
    # # }

    new_time <- current_time + step_duration

    if(date(new_time) != date(current_time)) {

      cat("=")
#
#       prop_night <- as.period(daylight_refs$sunset - daylight_refs$sunrise, unit = "hours")/hours(24)
#
#       # revise
#       daylight_refs <- suncalc::getSunlightTimes(lat = ref_loc[1], lon = ref_loc[2], date = as_date(new_time))
#
#       sim_day <- sim_day + 1

    }

    current_time <- new_time

    agent_update <- sf::st_sf(timestep = condition@timestep,
      body_mass = condition@body_mass,
      states_budget = list(condition@states_budget),
      energy_expenditure = condition@energy_expenditure,
      geometry = sf::st_sfc(condition@location)
    )

    #step_hist[i,] <- c(unlist(next_pos), current_state, current_expenditure, current_time, sim_day, energy_in_step)

  }
})

step_hist <- step_hist %>%
  sf::st_as_sf(coords = c(1, 2)) %>%
  mutate(current_state = factor(current_state), c_energy = cumsum(energy_out), sim_time = as_datetime(sim_time)) %>% #, sim_time = current_time + seconds(sim_time)) %>%
  st_set_crs(crs_utm31)

# ggplot() +
#   tidyterra::geom_spatraster(data = coast_repel$slope, alpha = 0.5) +
#   tidyterra::geom_spatraster(data = bound_repel$slope, alpha = 0.5) +
#   geom_sf(data = step_hist, aes(col = current_state), alpha = 0.5)

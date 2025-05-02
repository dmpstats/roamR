#' Update agent's condition
#'
#'
#' Given the current condition of the agent,
update_agent <- function(agent, move_fn, states_fn, crs, ...){


  agent@condition@timestep <- agent@condition@timestep + 1L

  location(agent) <- move_fn(current_loc)

  body_mass(agent) <- body_mass(agent) + estimate_mass_change()

  # Update agent@history
  new_row <- sf::st_sf(
    timestep = agent@condition@timestep,
    body_mass = body_mass(agent),
    states_budget = agent@condition@states_budget,
    energy_expenditure = agent@condition@energy_expenditure,
    geometry = sf::st_sfc(location(agent), crs = crs)
  )

  history(agent) <- rbind(history(agent), new_row)

  # output
  agent
}





estimate_mass_change <- function(x = NULL){

  mass_change <- runif(1, -10, 10)

  units::set_units(mass_change, "g")
}

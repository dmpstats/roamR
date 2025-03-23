bioss_run_sim <- function(in_agent, in_species, in_ibm, in_ibm_config, in_density = density_map){

  current_time <- in_ibm_config@start_date + days(1)

  step_duration <- days(1)

  current_month <- month(current_time)

  sst_month <- month(st_dimensions(in_ibm@drivers$sst@stars_obj)$time$values)

  night_proportion <- 1-(geosphere::daylength(lat = 56.18, doy = yday(current_time)))/24

  destination <- sample_cell(in_density, 1)

  while(current_time <= in_ibm_config@end_date){

    new_time <- current_time + step_duration

    in_sst <- st_extract(in_ibm@drivers$sst@stars_obj,
                         st_sfc(in_agent@condition@location, crs = in_ibm_config@ref_sys))$sst[which(sst_month == current_month)]

    if(lubridate::date(new_time) != lubridate::date(current_time)) {

      energy_profile <- calc_day_cost(in_agent = in_agent, in_species = in_species,
                                      in_ibm = in_ibm, sst = in_sst,
                                      intake = units::set_units(589.5, "kJ/h"))

      # existing activity profile - store
      energy_expenditure <- sum((energy_profile$prop*24) * energy_profile$unit_cost) %>%
        units::drop_units() %>%
        units::set_units(., "kJ")

      wt_gain <- units::drop_units(energy_expenditure) * 0.072 %>%
        units::set_units(., "g")

      # update activity profile for use in t+1
      nudge_states <- state_balance(in_states = energy_profile[1:4,], night_proportion = night_proportion,
                                    energy_target = units::set_units(1.14, "kJ/h"))

      in_agent@condition@states_budget[1:4] <- nudge_states %>%
        units::set_units(., 1) %>%
        as.list()


    }

    if(lubridate::month(new_time) != lubridate::month(current_time)) {

      destination <- sample_cell(in_density, 1)

    }

    current_time <- new_time
    in_agent@condition@location[1:2] <- destination[1:2]
    in_agent@condition@body_mass <- in_agent@condition@body_mass + wt_gain
    in_agent@condition@timestep <- in_agent@condition@timestep + integer(1)

    agent_update <- sf::st_sf(timestep = in_agent@condition@timestep,
                              body_mass = in_agent@condition@body_mass,
                              states_budget = list(in_agent@condition@states_budget),
                              energy_expenditure = energy_expenditure,
                              geometry = sf::st_sfc(in_agent@condition@location, crs = sf::st_crs(in_agent@history$geometry))
    )

    in_agent@history <- in_agent@history %>% bind_rows(agent_update)

  }

  in_agent

} # eof

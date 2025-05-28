#' Run a minimal simulation for BioSS analysess
#'
#' @param in_agent A roamR object of class Agent
#' @param in_species A roamR object of class Species
#' @param in_ibm A roamR object of class IBM
#' @param in_ibm_config A roamR object of class IBMConfig
#' @param in_density A stars object density map (raster) - projected as per ibm config and cropped to AoC
#'
#' @returns A roamR object of class Agent that has been simulated over time
#' @export
#'
#' @examples TBD
bioss_run_sim <- function(in_agent, in_species, in_ibm, in_ibm_config, mean_intake, impact){

  in_density <- in_ibm@drivers$dens@stars_obj
  impact_map <- in_ibm@drivers$energy_imp@stars_obj
  in_imp_density <- in_ibm@drivers$dens_imp@stars_obj
  sst_map <- in_ibm@drivers$sst@stars_obj


  if(impact == F){

    agent_imp_resp <- 0

  } else {

    agent_imp_resp <- in_agent@properties@move_influences$owf_imp

  }

  current_time <- in_ibm_config@start_date + lubridate::days(1)

  step_duration <- lubridate::days(1)

  current_month <- lubridate::month(current_time)

  sst_month <- lubridate::month(stars::st_dimensions(sst_map)$time$values)

  dens_month <- lubridate::month(stars::st_dimensions(in_density)$month$values)

  night_proportion <- 1-(geosphere::daylength(lat = 56.18, doy = lubridate::yday(current_time)))/24

  e_intake <- mean_intake


  if(impact == T & agent_imp_resp == 1){

    in_density <- in_imp_density

  }

  in_sst <- stars::st_extract(sst_map,
                              sf::st_sfc(in_agent@condition@location,
                              crs = in_ibm_config@ref_sys))$sst[which(sst_month == current_month)]

  current_density <- in_density |>
    dplyr::filter(month == current_month)

  current_density <- current_density[drop = T]

  noimp_density <- in_ibm@drivers$dens@stars_obj |>
    dplyr::filter(month == current_month)

  noimp_density <- noimp_density[drop = T]

  path_ind <- 1

  mv_path <- roamR::path_calc(density_map = noimp_density, imp_dens = current_density, agent = in_agent)

  while(current_time <= in_ibm_config@end_date){

    in_agent@condition@location <- sf::st_point(mv_path[[1]][path_ind, ])

    path_ind <- path_ind + 1

    new_time <- current_time + step_duration

    sst_query <- stars::st_extract(sst_map,
                         sf::st_sfc(in_agent@condition@location,
                                    crs = in_ibm_config@ref_sys))$sst[which(sst_month == current_month)]

    if(!is.na(sst_query)){in_sst <- sst_query}

    if(lubridate::date(new_time) != lubridate::date(current_time)) {

      if(impact == T){

        impact_p <- stars::st_extract(impact_map,
                                    sf::st_sfc(in_agent@condition@location,
                                    crs = in_ibm_config@ref_sys))$density[which(dens_month == current_month)]

        impact_p <- ifelse(is.na(impact_p), 1, impact_p) # if foraging in footprint

        e_intake <- mean_intake * impact_p

      }

      e_intake <- units::set_units(e_intake, "kJ/h")

      energy_profile <- roamR::calc_day_cost(in_agent = in_agent, in_species = in_species,
                                      in_ibm = in_ibm, sst = in_sst,
                                      intake = e_intake)

      # existing activity profile - store
      energy_expenditure <- sum((energy_profile$prop*24) * energy_profile$unit_cost) |>
        units::drop_units() |>
        units::set_units("kJ")

      wt_gain <- units::drop_units(energy_expenditure) * 0.072 |>
        units::set_units("g")

      # update activity profile for use in t+1

      nudge_states <- roamR::state_balance(in_states = energy_profile[1:4,],
                                           night_proportion = night_proportion,
                                           current_e = in_agent@condition@energy_expenditure,
                                           energy_target = 1)

      in_agent@condition@states_budget[1:4] <- nudge_states |>
        units::set_units(1) |>
        as.list()


    }

    if(lubridate::month(new_time) != lubridate::month(current_time)) {

      current_month <- lubridate::month(current_time)

      current_density <- in_density |>
        dplyr::filter(month == current_month)

      current_density <- current_density[drop = T] # drop unneeded dimensions

      noimp_density <- in_ibm@drivers$dens@stars_obj |>
        dplyr::filter(month == current_month)

      noimp_density <- noimp_density[drop = T]

      mv_path <- roamR::path_calc(density_map = noimp_density, imp_dens = current_density, agent = in_agent)

      path_ind <- 1

    }

    current_time <- new_time
    in_agent@condition@body_mass <- in_agent@condition@body_mass + wt_gain
    in_agent@condition@timestep <- in_agent@condition@timestep + as.integer(1)
    in_agent@condition@energy_expenditure <- in_agent@condition@energy_expenditure + energy_expenditure

    agent_update <- sf::st_sf(timestep = in_agent@condition@timestep,
                              body_mass = in_agent@condition@body_mass,
                              states_budget = list(in_agent@condition@states_budget),
                              energy_expenditure = in_agent@condition@energy_expenditure,
                              geometry = sf::st_sfc(in_agent@condition@location, crs = sf::st_crs(in_agent@history$geometry))
    )

    in_agent@history <- in_agent@history |>
      dplyr::bind_rows(agent_update)

  }

  in_agent@history <- in_agent@history |>
    dplyr::mutate(body_mass = stats::ksmooth(x = timestep, y = body_mass, kernel = "normal", bandwidth = 14)$y)

  in_agent

} # eof

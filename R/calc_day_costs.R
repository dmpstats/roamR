#' Calc day energy costs
#' For a given activity profile and series of cost functions (all directed at BioSS examples currently)
#'
#' @param in_agent A roamR object of class agent
#' @param in_species A roamR object of class species
#' @param in_ibm A roamR object of class ibm
#' @param intake The intake energy associated with feeding (kJ/h)
#' @param sst Numeric sea surface temperature
#'
#' @returns A data frame of costs for each state
#' @export
#'
#' @examples TBD
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





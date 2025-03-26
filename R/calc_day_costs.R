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

  costs <- list(flight = -roamR::flight_cost_fn(species = in_species),
                dive = -roamR::dive_cost_fn(species = in_species, t_dive = units::set_units(1.05, "min")) + intake,
                active = -roamR::active_water_cost_fn(sst = sst, species = in_species),
                inactive = -roamR::inactive_water_cost_fn(sst = sst, species = in_species),
                colony = -roamR::colony_cost_fn(species = in_species)) |>
    as.data.frame() |>
    tidyr::pivot_longer(names_to = "state", values_to = "unit_cost", tidyr::everything())


  in_agent@condition@states_budget |>
    as.data.frame() |>
    units::drop_units() |>
    tidyr::pivot_longer(names_to = "state", values_to = "prop", tidyr::everything()) |>
    dplyr::mutate(time = units::set_units(prop*24, "h")) |>
    dplyr::left_join(costs, by = "state") |>
    dplyr::mutate(day_cost = time * unit_cost)

}





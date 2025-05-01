#' The cost functions for each activity - specifically for initial BioSS run. BC has branched general case
#'
#' @param t_dive Dive time in minutes
#' @param species A roamR object of class species
#' @param sst Numeric sea surface temperature
#'
#' @returns All return an energetic cost in kJ/hr
#' @export
#'
#' @examples TBD
flight_cost_fn <- function(species){

  rand_par <- generate(species@states_profile$flight@energy_cost, 1) |>
    units::drop_units()

  (max(rand_par, 1)) |>
    units::set_units("kJ/h")

}



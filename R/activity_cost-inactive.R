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

inactive_water_cost_fn <- function(sst, species){

  rand_par <- generate(species@states_profile$inactive@energy_cost, 1) |>
    units::drop_units()

  (rand_par-(2.75*sst)) |>
    units::set_units("kJ/h")

}



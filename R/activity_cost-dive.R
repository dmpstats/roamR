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
dive_cost_fn <- function(t_dive, species){

  x <- units::drop_units(t_dive)

  rand_par <- generate(species@states_profile$dive@energy_cost, 1) |>
    units::drop_units()

  (max(rand_par*sum(1-exp(-x/1.23))/sum(x)*60, 1)) |>
    units::set_units("kJ/h")

}

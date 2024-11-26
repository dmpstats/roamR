#setClassUnion("SpeciesOrNull", c("Species", "NULL"))

#' Class `<Environment>`
#'
#' `<Environment>` is an S4 class that encapsulates the environment-level features of
#' the geographical area within the IBM's area of calculation (AOC). These
#' features are designed to interact with simulated Agents, influencing
#' their spatial movement, distribution, behaviour and physiological condition
#' over time
#'
#' @slot terrain object of class `<XY>` or `<POLYGON>`, representing land
#'   boundaries (e.g. mainland and/or islands) within the AOC
#' @slot bathymetry object of class `<stars>`, specifying the bathymetric data
#'   within the AOC
#' @slot prey object of class `<stars>`, providing prey density maps within the
#'   AOC, at discrete time points throughout the simulated period
#' @slot sst object of class `<stars>`, containing sea surface temperature maps
#'   within the AOC, at discrete time points  throughout the simulated period
#' @slot salinity object of class `<stars>`, containing sea salinity maps within
#'   the AOC, at discrete time points throughout the simulated period
#'
#' @include class-Species.R
#'
#' @export
#'
methods::setClass(
  Class = "Environment",
  slots = list(
    terrain = "XY",
    bathymetry = "stars",
    prey = "Species",
    sst = "stars",
    salinity = "stars"
  ),
  prototype = list(
    terrain = sf::st_polygon(),
    bathymetry = stars::st_as_stars(matrix(NA)),
    prey = new("Species"),
    sst = stars::st_as_stars(matrix(NA)),
    salinity = stars::st_as_stars(matrix(NA))
  )
)





#' Create a `<Environment>` object
#'
#' Helper function to construct instances of <[`Environment-class`]> objects
#'
#' @param terrain object of class `<XY>` or `<POLYGON>`, representing land
#'   boundaries (e.g. mainland and/or islands) within the AOC
#' @param bathymetry object of class `<stars>`, specifying the bathymetric data
#'   within the AOC
#' @param prey object of class `<stars>`, providing prey density maps within the
#'   AOC, at discrete time points throughout the simulated period
#' @param sst object of class `<stars>`, containing sea surface temperature maps
#'   within the AOC, at discrete time points  throughout the simulated period
#' @param salinity object of class `<stars>`, containing sea salinity maps
#'   within the AOC, at discrete time points throughout the simulated period
#'
Environment <- function(terrain = NA,
                        bathymetry = NA,
                        prey = NA,
                        sst = NA,
                        salinity = NA){

  if(is.na(terrain)) terrain <- sf::st_polygon()
  if(is.na(bathymetry)) bathymetry <- stars::st_as_stars(matrix(NA))
  if(is.na(prey)) prey <- new("Species")
  if(is.na(sst)) sst <- stars::st_as_stars(matrix(NA))
  if(is.na(salinity)) salinity <- stars::st_as_stars(matrix(NA))

  new(
    "Environment",
    terrain = terrain,
    bathymetry = bathymetry,
    prey = prey,
    sst = sst,
    salinity = salinity
  )
}



# # #' `show` method for `<Environment>`
# methods::setMethod(
#   "show",
#   "Environment",
#                    )
#



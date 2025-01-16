#setClassUnion("SpeciesOrNull", c("Species", "NULL"))

#' Class `<Habitat>`
#'
#' `<Habitat>` is an S4 class that encapsulates the Habitat-level features of
#' the geographical area within the IBM's area of calculation (AOC). These
#' features are designed to interact with simulated Agents, influencing their
#' spatial movement, distribution, behaviour and physiological condition over
#' time
#'
#' @slot terrain object of class `sf` or `sfc`, `sfc_POLYGON`, containing
#'   coastline polygon(s) within the AOC
#' @slot bathymetry object of class `<stars>`, specifying the bathymetric data
#'   within the AOC
#' @slot prey object of class `<stars>`, providing prey density maps within the
#'   AOC, at discrete time points throughout the simulated period
#' @slot sst object of class `<stars>`, containing sea surface temperature maps
#'   within the AOC, at discrete time points  throughout the simulated period
#' @slot sss object of class `<stars>`, containing sea surface salinity maps within
#'   the AOC, at discrete time points throughout the simulated period
#' @slot wind object of class `<stars>`, containing rasters of wind speed and
#'   direction within the AOC, at discrete time points throughout the simulated
#'   period
#'
#' @include class-Species.R s4_utils.R
#'
#' @seealso Helper function [Habitat()] to create `<Habitat>` objects
#'
#' @export
#'
methods::setClass(
  Class = "Habitat",
  slots = list(
    terrain = "sfc",
    bathymetry = "stars",
    prey = "Species",
    sst = "stars",
    sss = "stars",
    wind = "stars"
  ),
  prototype = list(
    terrain = sf::st_polygon() |> sf::st_sfc(),
    bathymetry = stars::st_as_stars(matrix(NA)),
    prey = Species(),
    sst = stars::st_as_stars(matrix(NA)),
    sss = stars::st_as_stars(matrix(NA)),
    wind = stars::st_as_stars(matrix(NA))
  )
)





#' Create a `<Habitat>` object
#'
#' Helper function to construct instances of <[`Habitat-class`]> objects
#'
#' @param terrain object of class `sf` or `sfc`, `sfc_POLYGON`, containing
#'   coastline polygon(s) within the AOC.
#' @param bathymetry object of class `<stars>`, specifying the bathymetric data
#'   within the AOC
#' @param prey object of class `<stars>`, providing prey density maps within the
#'   AOC, at discrete time points throughout the simulated period
#' @param sst object of class `<stars>`, containing sea surface temperature maps
#'   within the AOC, at discrete time points  throughout the simulated period
#' @param sss object of class `<stars>`, containing sea salinity maps
#'   within the AOC, at discrete time points throughout the simulated period
#' @param wind object of class `<stars>`, containing rasters of wind speed and
#'   direction within the AOC, at discrete time points throughout the simulated
#'   period
#'
Habitat <- function(terrain = NULL,
                    bathymetry = NULL,
                    prey = NULL,
                    sst = NULL,
                    sss = NULL,
                    wind = NULL){

  # Null input handling
  if(is.null(terrain)) terrain <- sf::st_polygon() |> sf::st_sfc()
  if(is.null(bathymetry)) bathymetry <- stars::st_as_stars(matrix(NA))
  if(is.null(prey)) prey <- Species(role = "prey")
  if(is.null(sst)) sst <- stars::st_as_stars(matrix(NA))
  if(is.null(sss)) sss <- stars::st_as_stars(matrix(NA))
  if(is.null(wind)) wind <- stars::st_as_stars(matrix(NA))

  # input validation  ---------------------------------
  if(is(terrain, "sf")) terrain <- sf::st_geometry(terrain) # extract sfc component from sf
  check_class(terrain, class = c("sfc"))
  check_class(bathymetry, class = "stars")
  check_class(prey, class = "Species")
  check_class(sst, class = "stars")
  check_class(sss, class = "stars")
  check_class(wind, class = "stars")

  if(!inherits(terrain, c("sfc_POLYGON", "sfc_MULTIPOLYGON"))){
    cli::cli_abort(c(
      "{.cls sf/sfc} object passed on to {.arg terrain} must contain simple feature(s) of type `POLYGON` or `MULTIPOLYGON`",
      "x" = "Object contains feature(s) of type {.code {sf::st_geometry_type(terrain)}}"
    ))
  }



  new(
    "Habitat",
    terrain = terrain,
    bathymetry = bathymetry,
    prey = prey,
    sst = sst,
    sss = sss,
    wind = wind
  )
}



# # #' `show` method for `<Habitat>`
# methods::setMethod(
#   "show",
#   "Habitat",
#                    )
#



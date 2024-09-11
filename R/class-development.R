#' Class `'Development'`
#'
#' `'Development'` is an S4 class for describing man-made structures - principally
#' windfarms, but could be extended to other types of structures. It stores the
#' type of structure, its spatial footprint, and its height. This class will
#' interact with the [Agent-class] class, via a movement modification.
#'
#' @slot type character string, the type of man-made structure (default: `"OWF"`
#'   for "Offshore Wnd Farm")
#' @slot boundary object of class `"XY"`, the polygon defining the footprint of
#'   the development structure
#' @slot height numeric value, the maximum height above sea level of the
#'   development structure (units: meters)
#'
#' @export

setClass(
  "Development",
  slots = list(
    type = "character",
    boundary = "XY",
    #boundary = "POLYGON",
    height = "numeric"
  ),
  prototype = list(
    type = "OWF",
    boundary = sf::st_polygon(),
    height = NA_real_
  )
)

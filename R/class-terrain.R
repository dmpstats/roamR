#' Class `'Terrain'`
#'
#' `'Terrain'` is an S4 class for specifying terrestrial features - e.g.
#' mainland and/or islands. It stores the type of structure and its spatial
#' delimitation. This class will interact with the [Agent-class] class, via
#' movement modification.
#'
#' @slot type character string, identifying the type of terrestrial feature
#'   (default: `"land"`)
#' @slot boundary object of class `"XY"`, a polygon delimiting the terrestrial
#'   feature
#'
#' @export

setClass(
  "Terrain",
  slots = list(
    type = "character",
    boundary = "XY"
  ),
  prototype = list(
    type = "land",
    boundary = sf::st_polygon()
  )
)

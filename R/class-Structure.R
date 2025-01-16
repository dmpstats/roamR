#' Class `<Structure>`
#'
#' `<Structure>` is an S4 class for describing man-made structures, such as
#' offshore windfarms, oil platforms, and other installations. It contains
#' information about the structure's type, spatial boundary (e.g. a footprint,
#' buffer area), and height.
#'
#' This class interacts with the [Species-class] class to define species-level
#' responses of traced agents to the structure, e.g. through a movement
#' modification or impact on specific behaviour.
#'
#' @slot id character string, a unique identifier of the structure.
#' @slot type character string, the type of man-made structure (e.g: `"OWF"`)
#' @slot boundary object of class `sf` or `sfc`, `sfc_POLYGON`, representing the
#'   polygon that defines the spatial footprint of the development structure
#' @slot height numeric, the maximum height of the structure above sea
#'   level (unit: meters)
#'
#' @include s4_management.R
#'
#' @export

methods::setClass(
  "Structure",
  slots = list(
    id = "character",
    type = "character",
    boundary = "sfc",
    height = "units"
  ),
  prototype = list(
    id = NA_character_,
    type = NA_character_,
    boundary = sf::st_polygon() |> sf::st_sfc(),
    height = units::set_units(NA, "m")
  )
)



#' Create a `<Structure>` object
#'
#' Helper function to construct instances of <[`Structure-class`]> objects
#'
#' @param id character string, a unique identifier or name for the structure
#' @param type character string, the type of man-made structure or influence
#'   (see 'Usage' section for available options).
#' @param boundary object of class `sf` or `sfc`, `sfc_POLYGON`, representing the
#'   polygon that defines the spatial footprint of the development structure
#' @param height numeric or object of class `<units>`, the maximum height of the
#'   structure above sea level (unit: meters)
#'
#' @return a `<Structure>` S4 object
#'
#' TODO: examples
#'
#' @export
Structure <- function(id = NA_character_,
                      type = c("owf", "traffic", "fishing"),
                      boundary = NULL,
                      height = NA_real_
                      ){

  # Null input handling
  if(is.null(boundary)) boundary <- sf::st_polygon() |> sf::st_sfc()

  # input validation  ---------------------------------
  if(is(boundary, "sf")) boundary <- sf::st_geometry(boundary) # extract sfc component from sf
  check_class(boundary, class = c("sfc"))

  type <- rlang::arg_match(type)

  if(!inherits(boundary, c("sfc_POLYGON", "sfc_MULTIPOLYGON"))){
    cli::cli_abort(c(
      "{.cls sf/sfc} object passed on to {.arg boundary} must contain simple feature(s) of type `POLYGON` or `MULTIPOLYGON`",
      "x" = "Object contains feature(s) of type {.code {sf::st_geometry_type(boundary)}}"
    ))
  }

  # units as expected
  if(is.numeric(height)){
    height <- units::set_units(height, "m")
  }

  # construct a new instance of <ModelConfig>
  methods::new(
    "Structure",
    id = id,
    type = type,
    boundary = boundary,
    height = height
  )
}





setGeneric("height", function(x) standardGeneric("height"))
setMethod("height", "Structure", function(x) x@height)

setGeneric("boundary", function(x) standardGeneric("boundary"))
setMethod("boundary", "Structure", function(x) x@boundary)


methods::setValidity("Structure", function(object) {

  if(!inherits(boundary(object), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))){
    "Slot @boundary must be an object of class <sfc_POLYGON> or <sfc_MULTIPOLYGON>"
  } else if(length(height(object)) > 1){
    "Slot @height must be of length 1"
  } else {
    TRUE
  }
})

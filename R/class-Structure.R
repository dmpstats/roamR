
#setClassUnion("numericORunits", c("numeric", "units"))

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
#' @slot boundary object of class `"XY"`, representing the polygon that defines
#'   the spatial footprint of the development structure
#' @slot height numeric, the maximum height of the structure above sea
#'   level (unit: meters)
#' @slot buffers numeric vector, defines buffer distances from the structure's
#'   boundary to be considered in the simulation (unit: meters)
#'
#' @include s4_management.R

#' @export

methods::setClass(
  "Structure",
  slots = list(
    id = "character",
    type = "character",
    boundary = "XY",
    height = "units",
    buffers = "units"
  ),
  prototype = list(
    id = NA_character_,
    type = NA_character_,
    boundary = sf::st_polygon(),
    height = NA_real_,
    buffers = NA_real_
  )
)



#' Create a `<Structure>` object
#'
#' Helper function to construct instances of <[`Structure-class`]> objects
#'
#' @param id character string, a unique identifier or name for the structure
#' @param type character string, the type of man-made structure (e.g: `"OWF"`)
#' @param boundary object of class `"XY"`, representing the polygon that defines
#'   the spatial footprint of the development structure
#' @param height numeric or object of class `<units>`, the maximum height of the
#'   structure above sea level (unit: meters)
#' @param buffers numeric or object of class `<units>`, defines buffer distances
#'   from the structure's boundary to be considered in the simulation (unit:
#'   meters)
#'
#'
#' TODO: examples
#''
#'
#' @export

Structure <- function(boundary = sf::st_polygon(),
                      height = NA_real_,
                      id = NA_character_,
                      type = NA_character_,
                      buffers = NA_real_){

  # units as expected
  if(is.numeric(height)){
    height <- units::set_units(height, "m")
  }

  if(is.numeric(buffers)){
    buffers <- units::set_units(buffers, "m")
  }

  # construct a new instance of <ModelConfig>
  methods::new(
    "Structure",
    boundary = boundary,
    height = height,
    id = id,
    type = type,
    buffers = buffers
  )
}






setGeneric("height", function(x) standardGeneric("height"))
setMethod("height", "Structure", function(x) x@height)

setGeneric("boundary", function(x) standardGeneric("boundary"))
setMethod("boundary", "Structure", function(x) x@boundary)




methods::setValidity("Structure", function(object) {

  if(!is(boundary(object), "POLYGON")){
    "Slot @boundary must be an object of class <POLYGON>"
  } else if(length(height(object)) > 1){
    "Slot @height must be of length 1"
  } else {
    TRUE
  }
})

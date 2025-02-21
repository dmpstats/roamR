#' Class `<Driver>`
#'
#' `<Driver>` is an S4 class encapsulating habitat-level or man-made features
#' that interact with simulated agents, driving and/or affecting their
#' spatial movement, distribution, behaviour and physiological condition within
#' the IBM's area of calculation (AOC) over the simulated period.
#'
#' The class supports two main categories of drivers:
#'
#'  - `"habitat"` - specifying environmental or anthropogenic factors
#'    that define the habitat's baseline state (e.g. Sea surface temperature,
#'    salinity, bathymetry, distance from land)
#'
#'  - `"impact"` - refers to influencers that deviate from the baseline and
#'    evaluated for their effects on the agents' simulated histories (e.g.
#'    Offshore windfarm footprints, oil rigs, shipping corridors)
#'
#'  - `"model"` - refers to influencers specified for model operational purposes
#'  (e.g. AOC bounding box to confine simulated movements)
#'
#' This class is designed to accommodate two types of spatial-based data formats
#' (simple features and multidimensional datacubes) providing a flexible
#' framework to specify driver characteristics.
#'
#' `<Driver>` interacts with the [Species-class] class to define species-level
#' responses of traced agents to the driver, e.g. through a movement
#' modification or impact on specific behaviour.
#'
#' @slot id character string, a unique identifier for the driver.
#' @slot descr character string, general description of the driver.
#' @slot ann character string, annotation for referencing the driver for
#'   reporting purposes.
#' @slot type character string, the driver type within the context of the IBM.
#'   Currently supports one of two values: "habitat" or "impact".
#' @slot sf_obj object of class `<sf>`, the simple feature representing the spatial
#'   geometry of the driver, if applicable.
#' @slot sf_descr character string, a brief description of data contained in
#'   `sf_obj`
#' @slot stars_obj an object of class `<stars>`, a multidimensional array
#'   containing grid-type spatio-temporal attributes of the driver, such as e.g
#'   time-series of raster-type density surfaces, distance surfaces to landscape
#'   features or man-made structures/impacts. First 2 dimensions are expected to
#'   provide the spatial-grid properties of the density surfaces. The 3rd
#'   dimension specifies the temporal resolution of the data, while the 4th
#'   dimension relates to random draws (e.g. bootstrap samples) of the density
#'   surfaces.
#' @slot stars_descr character string, a brief description of the data
#'   contained in `stars_obj`
#' @slot obj_active character string, specifying whether `sf_obj` or `stars_obj` is
#'   active and being used in the simulation.
#'
#' @include s4_management.R
#'
#' @seealso Helper function [Driver()] to create `<Driver>` objects
#'
#' @export

methods::setClass(
  "Driver",
  slots = list(
    id = "character",
    type = "character", # "habitat", "impact", "model"
    descr = "character",
    ann = "character",
    sf_obj = "sf",
    sf_descr = "character",
    stars_obj = "stars",
    stars_descr = "character",
    obj_active = "character" # "sf", "stars"
  ),
  prototype = list(
    id = NA_character_,
    type = NA_character_,
    descr = NA_character_,
    ann = NA_character_,
    sf_obj = sf::st_sf(geometry = sf::st_sfc(sf::st_polygon())),
    sf_descr = NA_character_,
    stars_obj = stars::st_as_stars(matrix(NA)),
    stars_descr = NA_character_,
    obj_active = NA_character_
  )
)



#' Create a `<Driver>` object
#'
#' Helper function to construct instances of <[`Driver-class`]> objects
#'
#' @param id character string, a unique identifier for the driver.
#' @param descr character string, general description of the driver.
#' @param ann character string, annotation for referencing the driver for
#'   reporting purposes.
#' @param type character string, the driver type within the context of the IBM.
#'   Currently supports one of two values: "habitat" or "impact".
#' @param sf_obj object of class `<sf>`, the simple feature representing the spatial
#'   geometry of the driver, if applicable.
#' @param sf_descr character string, a brief description of data contained in
#'   `sf_obj`
#' @param stars_obj stars_obj an object of class `<stars>`, a multidimensional array
#'   containing grid-type spatio-temporal attributes of the driver, such as e.g
#'   time-series of raster-type density surfaces, distance surfaces to landscape
#'   features or man-made structures/impacts. First 2 dimensions are expected to
#'   provide the spatial-grid properties of the density surfaces. The 3rd
#'   dimension specifies the temporal resolution of the data, while the 4th
#'   dimension relates to random draws (e.g. bootstrap samples) of the density
#'   surfaces.
#' @param stars_descr character string, a brief description of the data
#'   contained in `stars_obj`
#' @param obj_active character string, specifying whether `sf_obj` or `stars_obj` is
#'   active and being used in the simulation.
#'
#' @return a `<Driver>` S4 object
#'
#' TODO: examples
#'
#' @export
Driver <- function(id = NA_character_,
                   type = c("habitat", "impact", "model"),
                   descr = NA_character_,
                   ann = id,
                   sf_obj = NULL,
                   sf_descr = NA_character_,
                   stars_obj = NULL,
                   stars_descr = NA_character_,
                   obj_active = c("sf", "stars")   ){

  # Null input handling ---------------------------------------------
  sf_obj <- sf_obj %||% sf::st_sf(geometry = sf::st_sfc(sf::st_polygon()))
  stars_obj <- stars_obj %||% stars::st_as_stars(matrix(NA))

  # input validation  -----------------------------------------------
  type <- rlang::arg_match(type)
  obj_active <- rlang::arg_match(obj_active)

  if(is(sf_obj, "sfc")) sf_obj <- sf::st_sf(geometry = sf_obj)
  check_class(sf_obj, class = c("sf"))

  check_class(stars_obj, class = "stars")

  # TODO: validation on dimensionality of `stars` objects (e.g. impose
  # attribution of 3rd dimension to temporal steps and 4th dimension for
  # iterations)

  # construct a new instance of <Driver> ---------------------------
  methods::new(
    "Driver",
    id = id,
    type = type,
    descr = descr,
    ann = ann,
    sf_obj = sf_obj,
    sf_descr = sf_descr,
    stars_obj = stars_obj,
    stars_descr = stars_descr,
    obj_active = obj_active
  )

}



# Validator -----------------------------------------------------
methods::setValidity("Driver", function(object) {

  errors <- character()

  if(length(object@id) > 1){
    err <- c(err, "Slot @id must be of length 1")
  }else if(length(object@ann) > 1){
    err <- c(err, "Slot @ann must be of length 1")
  }

  if(length(errors) == 0) TRUE else do.call(paste, list(errors, collapse = " "))

})








# Accessors -----------------------------------------------------

## @stars_obj
### getter
setGeneric("stars_obj", function(x) standardGeneric("stars_obj"))
setMethod("stars_obj", "Driver", function(x) x@stars_obj)
### setter
setGeneric("stars_obj<-", function(x, value) standardGeneric("stars_obj<-"))
setMethod("stars_obj<-", "Driver", function(x, value) {
  x@stars_obj <- value
  validObject(x)
  x
})


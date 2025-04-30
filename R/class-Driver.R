#' Class `<Driver>`
#'
#' `<Driver>` is an S4 class encapsulating habitat-level or man-made features
#' that interact with simulated agents, driving and/or affecting their spatial
#' movement and distribution, behaviour states, energetic costings and
#' physiological condition within the IBM's area of calculation (AOC) over the
#' simulated period.
#'
#' The class supports three main categories of drivers:
#'
#'  - `"habitat"` - specifying environmental or anthropogenic factors
#'    that define the habitat's baseline state (e.g. Sea surface temperature,
#'    salinity, bathymetry, distance from land).
#'
#'  - `"impact"` - refers to influencers that deviate from the status-quo and
#'    evaluated for their effects on the agents' simulated histories (e.g.
#'    Offshore windfarm footprints, oil rigs, shipping corridors)
#'
#'  - `"model"` - refers to influencers specified for model operational purposes
#'  (e.g. AOC bounding box to confine simulated movements).
#'
#' This class is designed to accommodate two types of spatial-based data formats
#' (simple features and multidimensional datacubes) providing a flexible
#' framework to specify driver characteristics.
#'
#' `<Driver>` interacts with the [Species-class] class to define species-level
#' responses of traced agents to the driver, e.g. through a movement
#' modification or impact on behaviour states and their energetics costs.
#'
#' @slot id character string, a unique identifier for the driver.
#' @slot descr descr character string, providing a general description of the driver.
#' @slot ann character string, annotation for referencing the driver for
#'   reporting purposes.
#' @slot type character string, the driver type within the context of the IBM.
#'   Currently supports one of two values: "habitat" or "impact".
#' @slot sf_obj object of class `<sf>`, the simple feature representing the spatial
#'   geometry of the driver, if applicable.
#' @slot sf_descr character string, a brief description of data contained in
#'   `sf_obj`.
#' @slot stars_obj an object of class `<stars>`, a multidimensional array
#'   containing grid-type spatio-temporal attributes of the driver. Typical
#'   examples include time-series of raster-type density surfaces, distance
#'   layers to landscape features or to man-made structures/impacts.
#' @stars_meta a list object containing metadata about `stars_obj` required for
#'   modelling purposes
#' @slot stars_descr character string, a brief description of the data
#'   contained in `stars_obj`.
#' @slot obj_active character string, flagging whether `sf_obj` or `stars_obj`
#'   is to be used as the active driver data during simulation.
#'
#' @include s4_management.R
#'
#'
#' @seealso Helper function [Driver()] to create `<Driver>` objects.
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
    stars_meta = "list",
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
    stars_meta = list(),
    stars_descr = NA_character_,
    obj_active = "none"
  )
)

#' Create a `<Driver>` object
#'
#' Helper function to construct instances of <[`Driver-class`]> objects
#'
#' @param id character string, a unique identifier for the driver.
#' @param descr character string, providing a general description of the driver.
#' @param ann character string, annotation for referencing the driver for
#'   reporting purposes.
#' @param type character string, the driver type within the context of the IBM.
#'   Currently supports 3 options: "habitat" (default), "impact" or "model" (see
#'   <[`Driver-class`]>).
#' @param sf_obj object of class `<sf>`, the simple feature representing the spatial
#'   geometry of the driver, if applicable.
#' @param sf_descr character string, a brief description of data contained in
#'   `sf_obj`.
#' @param stars_obj an object of class `<stars>`, a multidimensional array of
#'   grid-type spatio-temporal attributes of the driver (e.g. time-series of
#'   prey-density surfaces, SST, distance surfaces to landscape features or to
#'   man-made structures/impacts). The `<stars>` object must follow a specific
#'   structure to be compatible with the model - see the *Details* section
#'   below for formatting requirements.
#' @param stars_descr character string, providing a brief description of the data
#'   contained in `stars_obj`.
#' @param obj_active character string, flagging whether `sf_obj` or `stars_obj`
#'   is to be used as the active driver data during simulation.
#'
#'
#' @details
#' # `stars_obj` formatting requirements
#'
#' The `<stars>` object provided to the `stars_obj` argument must meet the
#' following structural requirements:
#'
#' - Must contain a **single attribute**, with values mapped to a valid unit of
#' measurement.
#'
#' - Must include **two raster dimensions**, comprising the spatial grid-type
#' properties of the data.
#'
#' - May include **one temporal dimension** representing the temporal
#' resolution or aggregation of the data. When present:
#'    - if dimension values are of type `<character>`, they are assumed to
#'   represent months and must match valid English month names or abbreviations
#'   (see [month.name] and [month.abb]).
#'   - if dimension values are of type `<numeric>`, the dimension must be named
#'   as one of the following its values must conform to the specified ranges:
#'     - `"month"`: integer values from 1 to 12;
#'
#'     - `"year"`: 4-digit integers (e.g., 1998, 2024);
#'     - `"quarter"`: integers from 1 to 4;
#'     - `"yearweek"`: integers from 1 to 52 or 53;
#'     - `"yearday"`: integers from 1 to 365 or 366.
#'
#' - May include **one iteration-type dimension**, typically used to represent
#'   replicate draws from bootstrap sampling or stochastic simulations. This dimension
#'   must:
#'      - Contain integer values;
#'      - Be named `"iter"`, `"boot"`, or `"sample"`
#'
#' - The full data cube must not exceed **four dimensions** in total: two raster
#' dimensions and up to two non-raster dimensions (temporal and/or iterative).
#'
#'
#' @return a `<Driver>` S4 object
#'
#' @examples
#'
#' library(sf)
#' library(stars)
#'
#' # mocked-up driver defined by an <sf> object
#' (d <- Driver(
#'   id = "isl",
#'   type = "habitat",
#'   descr = "A pit-stop island",
#'   ann = "Squared Island",
#'   sf_obj = st_sf(geom = st_sfc(st_polygon(list(matrix(c(0,0,10,0,10,10,0, 10,0,0), ncol=2, byrow=TRUE))))),
#'   sf_descr = "coastline",
#'   obj_active = "sf"
#' ))
#'
#' plot(d@sf_obj, main = d@ann, col = "lightblue")
#'
#'
#'
#' # mocked-up driver defined by a raster-type <start> object
#' sst <- array(rlnorm(5*5*3*10), dim = c(5, 5, 3, 10)) |>
#'   st_as_stars() |>
#'   st_set_dimensions(3, names = "month", values = month.abb[1:3]) |>
#'   st_set_dimensions(4, names = "iter") |>
#'   setNames("sst") |>
#'   dplyr::mutate(sst = units::set_units(sst, "degree_Celsius"))
#'
#' (s <- Driver(
#'   id = "sst",
#'   type = "habitat",
#'   descr = "Sea Surface Temperature",
#'   ann = "SST",
#'   stars_obj = sst,
#'   obj_active = "stars"
#' ))
#'
#' plot(s@stars_obj, main = s@ann)
#'
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
                   obj_active = c("none", "sf", "stars") ){

# -------------------------------------------------------------------------


  # Null input handling ---------------------------------------------
  sf_obj <- sf_obj %||% sf::st_sf(geometry = sf::st_sfc(sf::st_polygon()))
  stars_obj <- stars_obj %||% stars::st_as_stars(matrix(NA))

  # input validation  -----------------------------------------------
  type <- rlang::arg_match(type)
  obj_active <- rlang::arg_match(obj_active)

  if(is(sf_obj, "sfc")) sf_obj <- sf::st_sf(geometry = sf_obj)

  check_class(sf_obj, class = c("sf"))
  check_class(stars_obj, class = "stars")

  # check and classify dimensions' info of stars_obj for model ---------------------
  stars_meta <- introspect_stars(stars_obj)

  # Construct a new instance of <Driver> ---------------------------
  methods::new(
    "Driver",
    id = id,
    type = type,
    descr = descr,
    ann = ann,
    sf_obj = sf_obj,
    sf_descr = sf_descr,
    stars_obj = stars_obj,
    stars_meta = stars_meta,
    stars_descr = stars_descr,
    obj_active = obj_active
  )

}







# Validator -----------------------------------------------------
methods::setValidity("Driver", function(object) {

  err <- character()

  if(length(object@id) > 1){
    err <- c(err, "Slot @id must be of length 1")
  }else if(length(object@ann) > 1){
    err <- c(err, "Slot @ann must be of length 1")
  }

  # @obj_active  ----------------------
  val_actv <- c("none", "sf", "stars")

  if(object@obj_active %notin% val_actv){
    msg <- cli::format_inline("\n - @obj_active must take one of: {.val {vec_style(val_actv)}}.")
    err <- c(err, msg)
  }

  if(object@obj_active == "sf" && all(sf::st_is_empty(object@sf_obj))){
    msg <- cli::format_inline(
      "\n - @sf_obj must be populated with non-empty geometries when @obj_active ",
      "is {.val sf}."
    )
    err <- c(err, msg)
  }

  if(object@obj_active == "stars" && is_stars_empty(object@stars_obj)){
    msg <- cli::format_inline(
      "\n - @stars_obj must be non-empty when @obj_active is {.val stars}."
    )
    err <- c(err, msg)
  }


  # @stars_obj -----------------------

  if(!is_stars_empty(object@stars_obj)){

    ## units
    attr_has_units <- sapply(object@stars_obj, inherits, "units")
    if(any(attr_has_units == FALSE)){
      msg <- cli::format_inline(
        "\n - @stars_obj: attributes must have units. Attribute{?s} ",
        "{.val {names(attr_has_units == FALSE)}} {?is/are} uniteless.")
      err <- c(err, msg)
    }
  }

  if(length(err) == 0) TRUE else do.call(paste, list(err, collapse = " "))

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


## @sf_obj
### getter
setGeneric("sf_obj", function(x) standardGeneric("sf_obj"))
setMethod("sf_obj", "Driver", function(x) x@sf_obj)
### setter
setGeneric("sf_obj<-", function(x, value) standardGeneric("sf_obj<-"))
setMethod("sf_obj<-", "Driver", function(x, value) {
  x@sf_obj <- value
  validObject(x)
  x
})


## utils  ----

#' @include s4_utils.R
methods::setMethod("is_empty", "Driver", function(object){
  #length(object@driver_responses) == 0
  all(sf::st_is_empty(object@sf_obj)) && is_stars_empty(object@stars_obj)
})

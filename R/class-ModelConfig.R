#' Class `<ModelConfig>`
#'
#' `<ModelConfig>` is an S4 class containing the user-defined configuration for
#' the Individual-based Model (IBM), including the number of agents to simulate,
#' the spatio-temporal resolution of the model, the bounding box (extent) of the
#' area of interest (AOI), the simulation start and end date
#'
#' @slot n_agents integer, the number of agents to track within the simulation
#' @slot ref_sys object of class <`crs`>, defining the Coordinate Reference System to
#'   be applied to the IBM. Must be specified via [sf::st_crs()]
#' @slot aoc_bbx object of class <`bbox`>, specifying the area of calculation,
#'   i.e. the spatial bounding box within which simulation occurs.
#' @slot delta_x,delta_y numeric, the cell (pixel) size in the x and y
#'   dimensions, respectively. Assumed to take the same units as `ref_sys`.
#' @slot time_step character string, defines the temporal resolution of the
#'   model. Refer to the `by` argument in [`seq.Date()`] for string-based
#'   specification of time increments.
#' @slot start_date;end_date Date, respectively, defines the start and end
#'   dates for the simulation period

methods::setClass(
  Class = "ModelConfig",
  slots = list(
    n_agents = "numeric",
    ref_sys = "crs",
    aoc_bbx = "bbox",
    delta_x = "numeric",
    delta_y = "numeric",
    time_step = "character",
    start_date = "Date",
    end_date = "Date"
  ),
  prototype = list(
    n_agents = NA_integer_,
    ref_sys = sf::NA_crs_,
    aoc_bbx = sf::NA_bbox_,
    delta_x = NA_real_,
    delta_y = NA_real_,
    time_step = NA_character_,
    start_date = as.Date(NA),
    end_date = as.Date(NA)
  )
)



#' Create a `<ModelConfig>` object
#'
#' Helper function to define the model configuration of the IMB. It constructs
#' instances of <[`ModelConfig-class`]> objects
#'
#' @param n_agent integer, the number of agents to track within the simulation.
#' @param ref_sys object of class <`crs`>, specifies the Coordinate Reference
#'   System to be applied to the IBM, defined via [`sf::st_crs()`]
#' @param aoc_bbx a numeric vector or object of class <`bbox`>, specifies the
#'   spatial bounding box delimiting the area of calculation within which
#'   simulation occurs. If a numeric, expects a 4-length vector specifying
#'   `xmin`, `ymin`, `xmax` and `ymax` values.
#' @param delta_x,delta_y numeric, the cell (pixel) size in the x and y
#'   dimensions, respectively. Assumed to be in the same units as `ref_sys`.
#' @param time_step character string, defines the temporal resolution of the
#'   model. Refer to the `by` argument in [`seq.Date()`] for string-based
#'   specification of time increments.
#' @param start_date;end_date Date, respectively, defines the start and end
#'   dates for the simulation period
#'
#' @return an object of class <[ModelConfig-class]>
#'
#' TODO: (i) examples; (ii) unit-tests
#'
#' @export
ModelConfig <- function(n_agents = 100,
                        ref_sys = sf::st_crs(4326),
                        aoc_bbx = c(0, 100, 0, 100),
                        delta_x = 10,
                        delta_y = 10,
                        time_step = "1 day",
                        start_date = Sys.Date() - 5,
                        end_date = Sys.Date()){


  if(!inherits(ref_sys, "crs")){
    cli::cli_abort(c(
      "{.arg ref_sys} must be an object of class {.cls crs}, not {.cls {class(ref_sys)}}",
      "i" = "Use {.code sf::st_crs()} to specify a suitable CRS"
    ))
  }


  if(!is(aoc_bbx, "bbox")){
    if(!is.numeric(aoc_bbx)){
      cli::cli_abort("{.arg aoc_bbx} must must be a {.cls numeric} vector")
    }

    if(length(aoc_bbx) != 4){
      cli::cli_abort("{.arg aoc_bbx} must have length 4, not {length(aoc_bbx)}")
    }

    aoc_bbx <- structure(
      as.double(aoc_bbx),
      names = c("xmin", "ymin", "xmax", "ymax"),
      class = "bbox",
      crs = ref_sys)

  } else if(sf::st_crs(aoc_bbx) != ref_sys) {

    cli::cli_abort("{.arg aoc_bbx} and {.arg ref_sys} must refer to the same CRS")

  }


  # construct a new instance of <ModelConfig>
  methods::new(
    "ModelConfig",
    n_agents = n_agents,
    ref_sys = ref_sys,
    aoc_bbx = aoc_bbx,
    delta_x = delta_x,
    delta_y = delta_y,
    time_step = time_step,
    start_date = start_date,
    end_date = end_date
  )
}


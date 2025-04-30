#' Class `<VarDist>`
#'
#' `<VarDist>` is an S4 class that encapsulates the distributional properties of
#' a variable of interest. It allows variables to be defined n terms of their
#' probability distribution, sampling distribution, percentile distribution or a
#' fixed value. The class provides a structured approach to specifying input
#' values by allowing the quantification of uncertainty while ensuring proper
#' handling of measurement units.
#'
#' @slot distr an object of class
#'   [`<distribution>`][distributional::distributional]. Specifies the
#'   distribution of values of the variable, representing its expected value and
#'   uncertainty/variability.
#' @slot units a character string, defining the measurement units of the
#'   variable. Must be either a name (e.g. `"grams"`) or a symbol (e.g. `"m/s"`)
#'   that recognized by the "udunits" database (see
#'   [units::valid_udunits()]). If `NULL` (default) the variable is assumed to
#'   be unitless.
#'
#'
#' @details
#' `<VarDist>` objects extend the functionality of the commendable
#' [distributional][distributional::distributional] package by incorporating
#' measurement units, ensuring that variable values are interpreted and
#' processed correctly during simulation calculations.
#'
#' @seealso
#'  * Helper function [VarDist()] to construct `<varDist>` objects
#'  * Package [distributional][distributional::distributional] for access to and details
#' on a comprehensive selection of distributions.
#'
#' @include s4_management.R

methods::setClass(
  Class = "VarDist",
  slots = list(
    distr = "distribution",
    units = "character"
  ),
  prototype = list(
    distr = distributional::dist_missing(),
    units = NA_character_
  )
)



#' Create a `<VarDist>` object
#'
#' Helper function to construct an instance of a <[`VarDist-class`]> object,
#' which defines a variable of interest in terms of its probability
#' distribution, sampling distribution, percentile distribution or fixed point
#' estimate.
#'
#' @param distr either an object of class
#'   [`<distribution>`][distributional::distributional] or a numeric value.
#'   Specifies the distribution of values of the variable, representing its
#'   expected value and uncertainty/variability. If a numeric value is provided,
#'   the variable is assumed to be constant and will remain fixed throughout the
#'   simulation.
#' @param units a character string, defining the measurement units of the
#'   variable. Must be either a name (e.g. `"grams"`) or a symbol (e.g. `"m/s"`)
#'   that recognized by the "udunits" database (see
#'   [units::valid_udunits()]). If `NULL` (default) the variable is assumed to
#'   be unitless.
#'
#' @details
#' `<VarDist>` objects extend the functionality of the commendable
#' [distributional][distributional::distributional] package by integrating
#' measurement units, ensuring that variable values are interpreted and
#' processed correctly during simulation calculations.
#'
#' @seealso
#' Package [distributional][distributional::distributional] for access to and details
#' on a comprehensive selection of distributions.
#'
#' @returns an object of class `<VarDist>`
#'
#' @examples
#' library(distributional)
#'
#' # define a Normally distributed variable with units m/s
#' VarDist(dist_normal(mean = 23, sd = 2), "m/s")
#'
#'
#' # define a parameter with fixed value
#' VarDist(10, "m")
#'
#' # set variable's empirical distribution from a random sample (e.g a bootstrap)
#' boot <- rlnorm(100, 2, 1)
#' mass <- VarDist(dist_sample(list(boot)), "kg")
#' # re-sample 100 values
#' generate(mass, times = 100)
#'
#' @export
VarDist <- function(distr = NULL, units = NULL){

  # input pre-processing -------------------------------------

  ## variable with fixed values (i.e. non-random)
  #if(is.numeric(distr) & length(distr) == 1) distr <- distributional::dist_degenerate(distr)
  if(is.numeric(distr)) distr <- distributional::dist_degenerate(distr)

  ## NULL handling
  distr <- distr %||% distributional::dist_missing()
  units <- units %||% ""

  # input validation -----------------------------------------

  ## validate `units`
  check_units(units)

  ## length validation
  if(length(distr) > 1){
    cli::cli_abort(c(
      "{.arg distr} must be of lenght 1",
      "x" = "You've provided an object with length {length(distr)}"
    ), class = "err-arg-wrong-length")
  }

  if(length(units) > 1){
    cli::cli_abort(c(
      "{.arg units} must be of lenght 1",
      "x" = "You've provided an object with length {length(units)}"
    ), class = "err-arg-wrong-length")

  }

  # validate classes
  check_class(units, "character")

  # validate `distr`
  if(!distributional::is_distribution(distr)){
    cli::cli_abort(c(
      "{.arg distr} must be either a {.cls numeric} or {.cls distribution} object.",
      "x" = "You've supplied an object of class {.cls {class(distr)}}"
    ))
  }

  # construct a new instance of <VarDist>
  new("VarDist", distr = distr, units = units)
}




# Validator -----------------------------------------------------
methods::setValidity("VarDist", function(object) {

  err <- character()

  if(length(object@distr) > 1){
    err <- c(err, "\n- slot @distr must be of length 1")
  }else if(length(object@units) > 1){
    err <- c(err, "\n- slot @units must be of length 1")
  }

  if(length(err) == 0) TRUE else do.call(paste, list(err, collapse = " "))

})



# Methods -----------------------------------------------------

## Accessors ----

### @distr
setGeneric("distr", function(object) standardGeneric("distr"))
setMethod("distr", "VarDist", function(object) object@distr)

### @units
setMethod("units", "VarDist", function(x) x@units)


## Other ----

#' Empty assertion
#' @include s4_utils.R
methods::setMethod("is_empty", "VarDist", function(object){
  is.na(object@distr)
})


#' VarDist random sampler
#'
#' @include s4_utils.R
methods::setMethod("generate", "VarDist", function(object, times = 1){
  vals <- distributional::generate(object@distr, times)
  vals <- lapply(vals, units::set_units, object@units, mode = "standard")
  if(length(vals) == 1) vals[[1]] else vals
})





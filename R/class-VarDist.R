#' Class `<VarDist>`
#'
#'
#' `<VarDist>` is an S4 class that encapsulates the distributional properties of
#' a variable of interest. Variables can be specified in terms of their
#' probability distribution, sampling distribution, percentile distribution or a
#' fixed value. Intended to provide a structured way to specify input values and
#' quantify the uncertainty associated with their estimates.
#'
#' @slot distr an object of class `<distribution>` (from package
#'   `{distributional}`), specifying the distribution of values of the variable
#' @slot units a character strintg, providing the measurement units the
#'   variable.  Must be either a name (e.g. `"grams"`) or a symbol (e.g.
#'   `"m/s"`) that is currently recognized by the udunits database (see
#'   [units::valid_udunits()])
#'
#' @seealso Helper function [VarDist()] to construct `<varDist>` objects
#'
#' @include s4_management.R
#'
#' @export

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
#' Helper function to construct instances of <[`VarDist-class`]> objects,
#' enabling the definition of variables of interest in terms of their probability
#' distributions, sampling distributions, percentile distributions or fixed
#' point estimates.
#'
#' @param distr an object of class `<distribution>` (from package
#'   `{distributional}`), specifying the distribution of values of the variable
#' @param units a character string, indicating the measurement units the
#'   variable. Must be either a name (e.g. `"grams"`) or a symbol (e.g. `"m/s"`)
#'   that is currently recognized by the udunits database (see
#'   [units::valid_udunits()]). If `NULL` (default) the variable is assumed to
#'   be unitless.
#' @details
#' The `{distributional}` package provides a comprehensive selection of
#' statistical distributions through a simple and intuitive interface.
#' `<VarDist>` objects build on this functionality by incorporating measurement
#' units for variables, ensuring precise and accurate handling of values in
#' subsequent calculations and analyses.
#'
#' @seealso
#' For details on available distributions, see the documentation for the
#' [`distributional`](https://pkg.mitchelloharawild.com/distributional/index.html)
#' package.
#'
#' @returns an object of class `<VarDist>`
#'
#' @examples
#' library(distributional)
#'
#' # define a Normally distributed variable with units m/s
#' VarDist(dist_normal(mean = 23, sd = 2), "m/s")
#'
#' # define a parameter with fixed value
#' VarDist(10, "m")
#'
#' # set variable empirical distribution from a random sample (e.g a bootstrap)
#' x <- rlnorm(100, 2, 1)
#' mass <- VarDist(dist_sample(list(x)), "kg")
#' # resample 100 values
#' generate(mass, times = 100)
#'
#' @export
VarDist <- function(distr = NULL, units = NULL){

  if(length(units) > 1){
    cli::cli_abort(c(
      "{.arg units} must be of lenght 1",
      "x" = "You've provided an object with length {length(units)}"
    ))
  }

  # NULL handling
  distr <- distr %||% distributional::dist_missing()
  units <- units %||% ""

  # validate classes
  check_class(units, "character")

  # validate `distr`
  if(!distributional::is_distribution(distr)){
    cli::cli_abort(c(
      "{.arg distr} must be an object of class {.cls distribution}",
      "x" = "You've supplied an object of class {.cls {class(distr)}}"
    ))
  }

  # validate `units`
  check_units(units)

  # handle variable with fixed values (i.e. non-random)
  if(is.numeric(distr) & length(distr) == 1) distr <- distributional::dist_degenerate(distr)

  # construct a new instance of <VarDist>
  new("VarDist", distr = distr, units = units)
}




# Methods -----------------------------------------------------

## Accessors ----

### @distr
setGeneric("distr", function(object) standardGeneric("distr"))
setMethod("distr", "VarDist", function(object) object@distr)

### @units
setMethod("units", "VarDist", function(x) x@units)


#' @include s4_utils.R
methods::setMethod("is_empty", "VarDist", function(object){
  is.na(object@distr)
})


#' @include s4_utils.R
methods::setMethod("generate", "VarDist", function(object, times = 1){
  vals <- distributional::generate(object@distr, times)
  vals <- lapply(vals, units::set_units, object@units, mode = "standard")
  if(length(vals) == 1) vals[[1]] else vals
})





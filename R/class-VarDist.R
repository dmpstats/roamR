#' Class `<VarDist>`
#'
#'
#' `<VarDist>` is an S4 class that encapsulates the distributional properties of
#' a variable of interest. It defines the variable in terms of its probability
#' distribution its mean and coefficient of variation (CV). Intended to provide
#' a structured way to specify input values and quantify the uncertainty
#' associated with their estimates.
#'
#' @slot dist an object of class `<distribution>` (from package
#'   `{distributional}`), specifying the distribution of values of the variable
#' @slot units a character strintg, providing the measurement units the
#'   variable.  Must be either a name (e.g. `"grams"`) or a symbol (e.g.
#'   `"m/s"`) that is currently recognized by the udunits database (see
#'   [units::valid_udunits()])
#'
#'
#' @seealso Helper function [VarDist()] to construct `<varDist>` objects
#'
#' @include s4_management.R

methods::setClass(
  Class = "VarDist",
  slots = list(
    dist = "distribution",
    units = "character"
  ),
  prototype = list(
    dist = dist_missing(),
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
#' @param dist an object of class `<distribution>` (from package
#'   `{distributional}`), specifying the distribution of values of the variable
#' @param units a character string, indicating the measurement units the
#'   variable. Must be either a name (e.g. `"grams"`) or a symbol (e.g. `"m/s"`)
#'   that is currently recognized by the udunits database (see [units::valid_udunits()])
#'
#' @details
#' The `{distributional}` package provides a comprehensive selection of
#' statistical distributions through a simple and intuitive interface.
#' `<VarDist>` objects build on this functionality by incorporating measurement
#' units for variables, ensuring precise and accurate handling of values in
#' subsequent calculations and analyses.
#'
#' @seealso
#' For details on available distributions, see the documentation for the
#' [`{distributional}`](https://pkg.mitchelloharawild.com/distributional/index.html)
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
#' distributional::generate(mass@dist, times = 100) |>
#'   lapply(units::as_units, value = mass@units)
#'
#'
#' @export
VarDist <- function(dist = NA, units = NA){

  if(length(units) > 1){
    cli::cli_abort(c(
      "{.arg units} must be of lenght 1",
      "x" = "You've provided an object with length {length(units)}"
    ))
  }

  # NA handling
  if(is.na(units)) units <- ""
  if(is.na(dist)) dist <- dist_missing()

  # validate classes
  check_class(units, "character")

  # validate `dist`
  if(!distributional::is_distribution(dist)){
    cli::abort(c(
      "{.arg dist} must be an object of class {.cls distribution}",
      "x" = "You've supplied an object of class {.cls {class(dist)}}"
    ))
  }

  # validate `units`
  check_units(units)

  # handle variable with fixed values (i.e. non-random)
  if(is.numeric(dist) & length(dist) == 1) dist <- distributional::dist_degenerate(dist)

  # construct a new instance of <VarDist>
  new("VarDist", dist = dist, units = units)
}


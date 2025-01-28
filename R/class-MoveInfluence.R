#' `<MoveInfluence>`
#'
#' `<MoveInfluence>` is an S4 class for defining the influence of a given
#' driver on the movement of simulated agents. This class is intended to
#' be used as a sub-class of <[`DriverResponse-class`]>`
#'
#' This class allows specifying the probabilistic influence of a driver on
#' agent movement, the function governing the movement influence, and the type
#' of influence being modelled.
#'
#' @slot prob an object of class <[`VarDist-class`]>, expressing the
#'   probability, and associated uncertainty/variability, that an agent's
#'   movement will be influenced by a driver.
#' @slot fn a `function`, defining the influence function applied to modify
#'   agent movement based on the driver's spatial properties. Specifically, this
#'   function describes the relationship between the magnitude of the influence
#'   and the spatial rate of change of the driver. The function should take a
#'   single argument, expressing the driver's rate of change, and its output,
#'   expressing the magnitude of the influence, should be bounded between 0 (no influence) and
#'   1 (maximum influence).
#' @slot type A `character` string, specifying the type of influence being applied
#'   (e.g., "attraction", "repulsion"). This allows for categorization of
#'   movement influence.
#'
#'
#' @include class-VarDist.R
#'
methods::setClass(
  Class = "MoveInfluence",
  slots = list(
    prob = "VarDist",
    fn = "function",
    type = "character"
  ),
  prototype = list(
    prob = VarDist(),
    fn = function(x) 0
  )
)



#' Create a `<MoveInfluence>` object
#'
#' Helper function to construct instances of <[`MoveInfluence-class`]> objects
#'
#' @export
MoveInfluence <- function(prob = VarDist(),
                          fn = function(x) 0,
                          type = c("null", "attraction", "repulsion")
                          ){
  # input validation
  type <- rlang::arg_match(type)

  # TOOO
  # - check on valid distributions for `prob`, i.e. dist_binom or dist_degenerate

  new(
    "MoveInfluence",
    prob = prob,
    fn = fn,
    type= type
  )

}

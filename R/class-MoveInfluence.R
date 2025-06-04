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
#' @slot mode A `character` string, specifying how the driver is to be applied
#'   by the movement model. Currently supports 2 options:
#'    -  "vector-field" (default): TODO
#'    - "cell-value": TODO
#' @slot sim_stage A `character` string, specifying at which stage of the simulation is driver applied
#'   by the movement model. Currently supports three options:
#'    - "baseline" (default): TODO
#'    - "impact": TODO
#'    - "baseline-and-impact": TODO
#'
#' @include class-VarDist.R
#'
methods::setClass(
  Class = "MoveInfluence",
  slots = list(
    prob = "VarDist",
    fn = "function",
    type = "character",
    mode = "character",
    sim_stage = "character"
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
#' @param prob an object of class <[`VarDist-class`]>, expressing the
#'   probability, and associated uncertainty/variability, that an agent's
#'   movement will be influenced by a driver.
#' @param fn a `function`, defining the influence function applied to modify
#'   agent movement based on the driver's spatial properties. Specifically, this
#'   function describes the relationship between the magnitude of the influence
#'   and the spatial rate of change of the driver. The function should take a
#'   single argument, expressing the driver's rate of change, and its output,
#'   expressing the magnitude of the influence, should be bounded between 0 (no influence) and
#'   1 (maximum influence).
#' @param type A `character` string, specifying the type of influence being applied
#'   (e.g., "attraction", "repulsion"). This allows for categorization of
#'   movement influence.
#' @param mode A `character` string, specifying how the driver is to be applied
#'   by the movement model. Currently supports 2 options:
#'    -  "vector-field" (default): TODO
#'    - "cell-value": TODO
#' @param sim_stage A `character` string, specifying at which stage of the simulation is driver applied
#'   by the movement model. Currently supports three options:
#'    - "baseline" (default): TODO
#'    - "impact": TODO
#'    - "baseline-and-impact": TODO
#'
#' @export
MoveInfluence <- function(prob = VarDist(),
                          fn = function(x) 0,
                          type = c("null", "attraction", "repulsion"),
                          mode = c("vector-field", "cell-value"),
                          sim_stage = c("baseline", "impact", "baseline-and-impact")
                          ){
  # input validation
  type <- rlang::arg_match(type)
  mode <- rlang::arg_match(mode)
  sim_stage <- rlang::arg_match(sim_stage)

  # TOOO
  # - check on valid distributions for `prob`, i.e. dist_binom or dist_degenerate

  new(
    "MoveInfluence",
    prob = prob,
    fn = fn,
    type = type,
    mode = mode,
    sim_stage = sim_stage
  )

}




# Validator -----------------------------------------------------
methods::setValidity("MoveInfluence", function(object) {
  errors <- character()
  if(length(distr(object@prob)) > 1){
    msg <- cli::format_inline(
      "\n - slot @prob must specify a {.cls VarDist} object containing one single distribution."
    )
    errors <- c(errors, msg)
  }

  if(length(errors) == 0) TRUE else do.call(paste, list(errors, collapse = " "))
})




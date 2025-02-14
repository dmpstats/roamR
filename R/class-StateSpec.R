#' Class `<StateSpec>`
#'
#' An S4 class containing state properties of agents associated with a specific
#' animal behaviour or activity, such as energy expenditure, time allocation and
#' movement speed, at the individual level. `<StateSpec>` is designed to be a
#' sub-class of [Species-class], where the behaviour profile of a species is
#' defined as a list of `<StateSpec>` objects. This design allows for flexible
#' state specification for species of interest.
#'
#' Currently, only three state properties are supported: energy cost, time
#' budget, and movement speed. However, the class design allows for the addition
#' of new properties in the future
#
#' @slot id character string, a unique identifier for the state, denoting
#'   a specific behaviour or activity.
#' @slot energy_cost an object of class [VarDist-class], representing the
#'   energy cost (e.g. kJ/hour/grams) associated with the state.
#' @slot time_budget an object of class [VarDist-class], defining the
#'   typical time-activity budget (e.g. hours/day) for the state.
#' @slot speed an object of class [VarDist-class], specifying the travel speed
#'   (e.g. m/s) associated with the state, particularly for movement-type
#'   activities
#'
#' @seealso
#'  * [VarDist()] for defining `<VarDist>` objects
#'  * Helper function [StateSpec()] to construct `<StateSpec>` objects
#'
#' @include class-VarDist.R s4_management.R s4_utils.R utils.R
#'
#' @export

methods::setClass(
  Class = "StateSpec",
  slots = list(
    id = "character",
    energy_cost = "VarDist",
    time_budget = "VarDist",
    speed = "VarDist"
  ),
  prototype = list(
    id = NA_character_,
    energy_cost = VarDist(),
    time_budget = VarDist(),
    speed = VarDist()
  )
)




#' Create a `<StateSpec>` object
#'
#' Helper function to construct instances of <[`StateSpec-class`]> objects,
#' enabling the definition properties associated with a specific animal behaviour
#' or activity, such as energy expenditure, time allocation and movement speed,
#' at the individual level
#'
#' @param id character string, a unique identifier for the state, denoting
#'   a specific behaviour or activity.
#' @param energy_cost an object of class [VarDist-class], representing the
#'   energy cost (e.g. kJ/hour/grams) associated with the state.
#' @param time_budget an object of class [VarDist-class], defining the
#'   typical time-activity budget (e.g. hours/day) for the state.
#' @param speed an object of class [VarDist-class], specifying the speed
#'   (e.g. m/s) associated with the state, particularly for
#'   movement-type activities
#'
#' @seealso [VarDist()] for defining `<VarDist>` objects
#'
#' @return a `<StateSpec>` S4 object
#'
#' @export
StateSpec <- function(id = NA_character_,
                      energy_cost = VarDist(),
                      time_budget = VarDist(),
                      speed = VarDist()){

  # Input validation
  check_class(energy_cost, "VarDist")
  check_class(time_budget, "VarDist")
  check_class(speed, "VarDist")


  # construct a new instance of <StateSpec>
  new(
    "StateSpec",
    id = id,
    energy_cost = energy_cost,
    time_budget = time_budget,
    speed = speed
  )

}

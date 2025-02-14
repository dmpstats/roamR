#' Class `<BehaviourSpec>`
#'
#' An S4 class containing properties associated with a specific animal behaviour
#' or activity, such as energy expenditure, time allocation and movement speed,
#' at the individual level. `<BehaviourSpec>` is designed to be a sub-class of
#' <[`Species-class`]>, where the behaviour profile of a species is defined as
#' a list of `<BehaviourSpec>` objects. This design allows for flexible and
#' detailed behaviour specification for species of interest.
#'
#' Currently, only three behaviour properties are supported: energy cost, time
#' budget, and movement speed. However, the class design allows for the addition
#' of new properties in the future
#
#' @slot behav character string, the name of the behaviour or activity
#' @slot energy_cost an object of class <[`VarDist-class`]>, representing the
#'   energy cost (e.g. kJ/hour/grams) associated with the behaviour or activity.
#' @slot time_budget an object of class <[`VarDist-class`]>, defining the
#'   typical time-activity budget (e.g. hours/day) for the behaviour or activity.
#' @slot speed an object of class <[`VarDist-class`]>, specifying the travel speed
#'   (e.g. m/s) associated with the behaviour or activity, particularly for
#'   movement-type activities
#'
#' @seealso
#'  * [VarDist()] for defining <[`VarDist-class`]> objects
#'  * Helper function [BehaviourSpec()] to construct `<BehaviourSpec>` objects
#'
#' @include class-VarDist.R s4_management.R s4_utils.R utils.R
#'
#' @export

methods::setClass(
  Class = "BehaviourSpec",
  slots = list(
    behav = "character",
    energy_cost = "VarDist",
    time_budget = "VarDist",
    speed = "VarDist"
  ),
  prototype = list(
    behav = NA_character_,
    energy_cost = VarDist(),
    time_budget = VarDist(),
    speed = VarDist()
  )
)




#' Create a `<BehaviourSpec>` object
#'
#' Helper function to construct instances of <[`BehaviourSpec-class`]> objects,
#' enabling the definition properties associated with a specific animal behaviour
#' or activity, such as energy expenditure, time allocation and movement speed,
#' at the individual level
#'
#' @param behav character string, the name of the behaviour or activity
#'   (see 'Usage' section for available options).
#' @param energy_cost an object of class [VarDist-class], representing the
#'   energy cost (e.g. kJ/hour/grams) associated with the behaviour or activity.
#' @param time_budget an object of class [VarDist-class], defining the
#'   typical time-activity budget (e.g. hours/day) for the behaviour or activity.
#' @param speed an object of class [VarDist-class], specifying the speed
#'   (e.g. m/s) associated with the behaviour or activity, particuarly for
#'   movement-type activities
#'
#' @seealso [VarDist()] for defining `<VarDist>` objects
#'
#' @details
#' * Currently, only three behaviour properties are supported: energy cost, time
#' budget, and movement speed. However, the class design allows for the addition
#' of new properties in the future
#'
#' * To maintain consistency with the overall IBM framework underpinning `{roamR}`,
#' `behav` is confined to a fixed set of behaviours or activities (see available
#' options in the 'Usage' section).
#'
#'
#' @return a `<BehaviourSpec>` S4 object
#'
#' @export
BehaviourSpec <- function(behav = c("flying", "swimming", "diving", "foraging",
                                    "water_resting", "nest_attending",
                                    "travelling", "commuting", "other"),
                          energy_cost = VarDist(),
                          time_budget = VarDist(),
                          speed = VarDist()){

  # Input validation
  behav <- rlang::arg_match(behav)
  check_class(energy_cost, "VarDist")
  check_class(time_budget,  "VarDist")
  check_class(speed,  "VarDist")


  # construct a new instance of <BehaviourSpec>
  new(
    "BehaviourSpec",
    behav = behav,
    energy_cost = energy_cost,
    time_budget = time_budget,
    speed = speed
  )

}

#' Class `<State>`
#'
#' An S4 class representing the properties of an agent's behavioural or activity
#' state. Each `<State>` object defines characteristics such as energy
#' expenditure, time allocation, and movement speed at the individual level.
#'
#' `<State>` is designed as a sub-class of [Species-class], allowing a species'
#' state profile to be structured as a list of `<State>` objects. This flexible
#' design enables customized state specifications for different species.
#'
#' Currently, three state attributes are supported: energy cost, time budget,
#' and movement speed. However, the class design allows for future expansions to
#' accommodate additional state attributes.
#
#' @slot id character string, a unique identifier for the state, representing a
#'   specific behaviour or activity.
#' @slot energy_cost a <[`VarDist-class`]> object, defining the energy
#'   expenditure associated with the state (e.g. kJ/hour/grams).
#' @slot time_budget a <[`VarDist-class`]> object, defining the agent's typical
#'   time allocation to this state. It should be expressed as a relative length
#'   of time (e.g. hours/day).
#' @slot speed a <[`VarDist-class`]> object, specifying the movement speed
#'   associated with this state (e.g. m/s).
#'
#' @seealso
#'  * [VarDist()] for defining `<VarDist>` objects
#'  * Helper function [State()] to construct `<State>` objects
#'
#' @include class-VarDist.R s4_management.R s4_utils.R utils.R
#'
#' @export

methods::setClass(
  Class = "State",
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




#' Create a `<State>` object
#'
#' Helper function to construct instances of <[`State-class`]> objects, enabling
#' the specification of attributes specific to an agent's behavioural or
#' activity state. Each `<State>` object defines characteristics such as energy
#' expenditure, time allocation, and movement speed at the individual level.
#'
#'
#' @param id character string, a unique identifier for the state, representing a
#'   specific behaviour or activity.
#' @param energy_cost a <[`VarDist-class`]> object, defining the energy
#'   expenditure associated with the state (e.g. kJ/hour/grams).
#' @param time_budget a <[`VarDist-class`]> object, defining the agent's typical
#'   time allocation to this state. It should be expressed as a relative length
#'   of time (e.g. hours/day).
#' @param speed a <[`VarDist-class`]> object, specifying the movement speed
#'   associated with this state (e.g. m/s).
#'
#' @seealso [VarDist()] for defining `<VarDist>` objects
#'
#' @return a <[`State-class`]> S4 object
#'
#' @export
State <- function(id = NA_character_,
                  energy_cost = VarDist(),
                  time_budget = VarDist(),
                  speed = VarDist()){

  # Input validation
  check_class(energy_cost, "VarDist")
  check_class(time_budget, "VarDist")
  check_class(speed, "VarDist")


  # construct a new instance of <State>
  new(
    "State",
    id = id,
    energy_cost = energy_cost,
    time_budget = time_budget,
    speed = speed
  )

}




# Validator -----------------------------------------------------
methods::setValidity("State", function(object) {

  err <- c()

  # if(!is_empty(object@time_budget)){
  #   if(units(object@time_budget) == "") {
  #     msg <- cli::format_inline("\n - @time_budget: {.cls VarDist} slot @units must be specified.")
  #     err <- c(err, msg)
  #   }
  # }

  if(length(err) > 0){
    # need to collapse into single string for desired formatting
    do.call(paste, list(err, collapse = " "))
  } else{
    TRUE
  }
})

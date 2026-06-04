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
#' @slot id  <[`string`][character]> the unique identifier for the state, representing a
#'   specific behaviour or activity.
#' @slot type <[`string`][character]> specifies the functional category of the activity. This classifies states based on their role in the agent's life. Current accepted values are:
#'   - "foraging": encompasses both food searching and consumption.
#'   - "resting": covers sleep and other inactive behaviours.
#'   - "travelling": includes locomotion for non-foraging purposes (e.g., flying, running, swimming, or migrating).
#'   - "other": for any state not covered by the above categories.
#' @slot energy_cost <[`VarDist-class`]> the energy expenditure associated with the state (e.g. kJ/hour/grams).
#' @slot time_budget <[`VarDist-class`]> the agent's typical time allocation to this state, expressed as a relative duration (e.g. hours/day).
#' @slot speed <[`VarDist-class`]> the movement speed associated with this state (e.g. m/s).
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
    type = "character",
    energy_cost = "VarDist",
    time_budget = "VarDist",
    speed = "VarDist"
  ),
  prototype = list(
    id = NA_character_,
    type = NA_character_,
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
#' @param id  <[`string`][character]> the unique identifier for the state, representing a
#'   specific behaviour or activity.
#' @param type <[`string`][character]> specifies the functional category of the activity. This classifies states based on their role in the agent's life. Current accepted values are:
#'   - "foraging": encompasses both food searching and consumption.
#'   - "resting": covers sleep and other inactive behaviours.
#'   - "travelling": includes locomotion for non-foraging purposes (e.g., flying, running, swimming, or migrating).
#'   - "other": for any state not covered by the above categories.
#' @param energy_cost <[`VarDist-class`]> the energy expenditure associated with the state (e.g. kJ/hour/grams).
#' @param time_budget <[`VarDist-class`]> the agent's typical time allocation to this state, expressed as a relative duration (e.g. hours/day).
#' @param speed <[`VarDist-class`]> the movement speed associated with this state (e.g. m/s).
#'
#' @seealso [VarDist()] for defining `<VarDist>` objects
#'
#' @return a <[`State-class`]> S4 object
#'
#' @examples
#'
#'  # Create a <State> object representing a foraging state
#'  State(
#'   id = "foraging",
#'   type = "foraging",
#'   energy_cost = VarDist(distributional::dist_normal(4, 0.5), "kJ/hour/grams"),
#'   time_budget = VarDist(10, "hour/day"),
#'   speed = VarDist(distributional::dist_uniform(0.5, 1.5), "m/s")
#' )
#'
#' @export
State <- function(
  id = NA_character_,
  type = c("foraging", "resting", "travelling", "other"),
  energy_cost = VarDist(),
  time_budget = VarDist(),
  speed = VarDist()
) {
  # TODO: include alternative input formats on function's documentation
  type <- match.arg(type)

  speed <- as_vardist(speed, "m/s")
  energy_cost <- as_vardist(energy_cost, "kJ/hour/grams")
  time_budget <- as_vardist(time_budget, "hour/day")

  # Input validation
  check_class(id, "character")
  check_class(energy_cost, "VarDist")
  check_class(time_budget, "VarDist")
  check_class(speed, "VarDist")

  # construct a new instance of <State>
  new(
    "State",
    id = id,
    type = type,
    energy_cost = energy_cost,
    time_budget = time_budget,
    speed = speed
  )
}


# Validator -----------------------------------------------------
methods::setValidity("State", function(object) {
  err <- character()

  # if(!is_empty(object@time_budget)){
  #   if(units(object@time_budget) == "") {
  #     msg <- cli::format_inline("\n - @time_budget: {.cls VarDist} slot @units must be specified.")
  #     err <- c(err, msg)
  #   }
  # }

  if (length(err) > 0) {
    # need to collapse into single string for desired formatting
    do.call(paste, list(err, collapse = " "))
  } else {
    TRUE
  }
})

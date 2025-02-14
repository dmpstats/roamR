#' `<AgentCondition>`
#'
#' `<AgentCondition>` is an S4 class describing the status of an agent at the
#' end of a simulation `time_step` (e.g. end of day). It encapsulates key
#' spatio-temporal and physiological metrics, including location, body mass,
#' energy expenditure, state budgets, and survival status.
#'
#' Currently this class is intended for internal use within the model, and it is
#' not expected to be user-facing.
#'
#' @slot location object of class `XY`, specifying the agent's spatial
#'   coordinates at the end of the current time-step.
#' @slot grid_cell object of class `XY`, representing the AOC's grid-cell
#'   occupied by the agent.
#' @slot timestep integer, indicating the current simulation time-step index.
#' @slot timestamp a `<POSIXct>` object, the date-time at the end of the current
#'   time-step.
#' @slot body_mass a `<units>` object, defining the agent's body mass at the end
#'   of the current time-step.
#' @slot states_budget a named list, detailing the agent's time allocation
#'   across the different behavioural/activity states during the time-step. Each
#'   element (of type `<units>`) is named after a specific state and stores the
#'   duration (e.g. hours) the agent spent in that state.
#' @slot states_cost a named list, specifying the energy costs (e.g. KJ/hr/g)
#'   associated with each state defined in `state_budget`. List elements are of
#'   type `<units>`.
#' @slot energy_expenditure a `<units>` object, representing the total energy
#'   (e.g. KJ/g) expended across all states during the time-step.
#' @slot foraging_success a `<units>` object, the mass of prey (e.g. grams)
#'   consumed by the agent in the current time-step.
#' @slot mass_change_value a `<units>` object, capturing the net change in the
#'   agent’s body mass (e.g., grams) over the current time-step.
#' @slot mortality_prob numeric, the probability of the agent dying within the
#'   the current time-step (*currently ignored*).
#' @slot alive a logical value (`TRUE` if alive, `FALSE` if dead), indicating
#'   whether the agent has survived the time-step.
#' @slot track object of class `sf`, storing a time-series of location points
#'   tracking the agent's movement within the current time-step.
#'
#' @include s4_management.R
#'
#' @seealso
#' Helper function [AgentCondition()] to create `<AgentCondition>` objects
#'
methods::setClass(
  Class = "AgentCondition",
  slots = list(
    location = "XY",
    grid_cell = "XY",
    timestep = "integer",
    timestamp = "POSIXct",
    body_mass = "units",
    states_budget = "list",
    states_costs = "list",
    energy_expenditure = "units",
    foraging_success = "units",
    mass_change_value = "units",
    mortality_prob = "numeric",
    alive = "logical",
    track = "sf"
  ),
  prototype = list(
    location = sf::st_point(),
    grid_cell = sf::st_point(),
    timestep = NA_integer_,
    timestamp = as.POSIXct(NA),
    body_mass = units::set_units(NA, "g"),
    states_budget = list(),
    states_costs = list(),
    energy_expenditure = units::set_units(NA, "kJ/g"),
    foraging_success = units::set_units(NA, "g/day"),
    mass_change_value = units::set_units(NA, "g"),
    mortality_prob =  NA_real_,
    alive = NA,
    track = sf::st_sf(
      timestamp = as.POSIXct(NA),
      geom = sf::st_sfc(sf::st_point())
    )
  )
)



#' Create `<AgentCondition>` objects
#'
#' `AgentCondition()` is a helper function to construct instances of
#' `[AgentCondition-class]` objects, which carries the status of an agent at the
#' end of a simulation `time_step` (e.g. end of day).
#'
#' @param location object of class `XY`, specifying the agent's spatial
#'   coordinates at the end of the current time-step.
#' @param grid_cell object of class `XY`, representing the AOC's grid-cell
#'   occupied by the agent.
#' @param timestep integer, indicating the current simulation time-step index.
#' @param timestamp a `<POSIXct>` object, the date-time at the end of the current
#'   time-step.
#' @param body_mass a `<units>` object, defining the agent's body mass at the end
#'   of the current time-step.
#' @param states_budget a named list, detailing the agent's time allocation
#'   across the different behavioural/activity states during the time-step. Each
#'   element (of type `<units>`) is named after a specific state and stores the
#'   duration (e.g. hours) the agent spent in that state.
#' @param states_cost a named list, specifying the energy costs (e.g. KJ/hr/g)
#'   associated with each state defined in `state_budget`. List elements are of
#'   type `<units>`.
#' @param energy_expenditure a `<units>` object, representing the total energy
#'   (e.g. KJ/g) expended across all states during the time-step.
#' @param foraging_success a `<units>` object, the mass of prey (e.g. grams)
#'   consumed by the agent in the current time-step.
#' @param mass_change_value a `<units>` object, capturing the net change in the
#'   agent’s body mass (e.g., grams) over the current time-step.
#' @param mortality_prob numeric, the probability of the agent dying within the
#'   the current time-step (*currently ignored*).
#' @param alive a logical value (`TRUE` if alive, `FALSE` if dead), indicating
#'   whether the agent has survived the time-step.
#' @param track object of class `sf`, storing a time-series of location points
#'   tracking the agent's movement within the current time-step.
#'
#'
#' @export
#'
AgentCondition <- function(location = sf::st_point(),
                           grid_cell = sf::st_point(),
                           timestep = NA_integer_,
                           timestamp = as.Date(NA),
                           body_mass =  NA_real_,
                           states_budget = list(),
                           states_costs = list(),
                           mortality_prob =  NA_real_,
                           alive = NA,
                           track = sf::st_sf(
                             timestamp = as.POSIXct(NA),
                             geom = sf::st_sfc(sf::st_point())
                           )){

}




# getters
setGeneric("location", function(x) standardGeneric("location"))
setMethod("location", "AgentCondition", function(x) x@location)

setGeneric("body_mass", function(x) standardGeneric("body_mass"))
setMethod("body_mass", "AgentCondition", function(x) x@body_mass)



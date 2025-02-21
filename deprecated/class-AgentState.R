#' `<AgentState>`
#'
#' @description
#' `AgentState` is an S4 class for specifying an agent's current state in terms
#' of spatio-temporal activity and physiological condition at the end of the
#' current simulation `time_step` (e.g. end of day). It stores information such
#' as body mass, activity budgets, energy expenditure metrics, survival status,
#' etc
#'
#' Currently this is an "internal" class, i.e. it is not meant to be user-facing
#'
#' @slot location object of class `XY`, specifying the agent's current location
#'   (units/projection: TBD).
#' @slot grid_cell object of class `XY`, indicating the spatial coordinates of
#'   the grid-cell occupied by the agent
#' @slot timestep integer, the index of current simulation time-step
#' @slot timestamp a `POSIXct` object, the date-time at the end of the current
#'   simulation time-step
#' @slot body_mass an object of class `units`, the agent's current body mass
#' @slot activity_budget named list, defining the agent's time-in-activity
#'   budget in the current time-step. Each list element (of class `units`)
#'   represents an activity and the time the agent spends on it during this
#'   time-step (units: hours/time-step).
#' @slot activity_costs named list, specifying the energy costs (e.g. KJ/hr/g)
#'   associated with each activity in argument `activity_budget`. List elements
#'   should be of type `units`
#' @slot energy_expenditure a `units` object, total energy (e.g. KJ/g) spent on
#'   all activities by the end of the current time-step
#' @slot foraging_success a `units` object, the mass of prey consumed per
#'   time-step (units: g/time-step)
#' @slot mass_change_value numeric, specifying the change in the agent’s mass
#'   over the time-step (units: g)
#' @slot mortality_prob numeric, the probability of the agent dying within the
#'   the current time-step
#' @slot alive logical, indicating the agent's survival status (`TRUE` if
#'   alive, `FALSE` if dead)
#' @slot track object of class `sf`, containing time-location points that
#'   represent the agent's movement in the current time-step
#'
#' @include s4_management.R
#'
#' @export

methods::setClass(
  Class = "AgentState",
  slots = list(
    location = "XY",
    grid_cell = "XY",
    timestep = "integer",
    timestamp = "POSIXct",
    body_mass = "units",
    activity_budget = "list",
    activity_costs = "list",
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
    activity_budget = list(
      flight = NA,
      water_active = NA,
      water_inactive = NA,
      nest_resting = NA,
      diving = NA,
      other = NA),
    activity_costs = list(
      flight = NA,
      water_active = NA,
      water_inactive = NA,
      nest_resting = NA,
      diving = NA),
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


# getters
setGeneric("location", function(x) standardGeneric("location"))
setMethod("location", "AgentState", function(x) x@location)

setGeneric("body_mass", function(x) standardGeneric("body_mass"))
setMethod("body_mass", "AgentState", function(x) x@body_mass)



# # validator
# methods::setValidity("AgentState", function(object) {
#
#   if(object@){
#     "Slot @boundary must be an object of class <POLYGON>"
#   } else if(length(height(object)) > 1){
#     "Slot @height must be of length 1"
#   } else {
#     TRUE
#   }
# })



#' #' Create `<AgentState>` objects
#' #'
#' #' `AgentState()` is a helper function to construct instances of
#' #' `[AgentState-class]` objects, which stores an agent's current state in terms
#' #' of spatio-temporal activity and physiological condition at the end of the
#' #' current simulation `time_step` (e.g. end of day).
#' #'
#' #' @slot location object of class `XY`, specifying the agent's current location
#' #'   (units/projection: TBD).
#' #' @slot grid_cell object of class `XY`, indicating the spatial coordinates of
#' #'   the grid-cell occupied by the agent
#' #' @slot timestep integer, the index of current simulation time-step
#' #' @slot timestamp a `POSIXct` object, the date-time at the end of the current
#' #'   simulation time-step
#' #' @slot body_mass numeric, the agent's current body mass (units: g)
#' #' @slot activity_budget named list, defining the agent's time-in-activity budget in
#' #'   the current time-step. Each list element represents an activity and the
#' #'   time the agent spends on it during this time-step (units: hours/time-step).
#' #' @slot activity_costs named list, specifying the energy costs associated with each
#' #'   activity in argument `activity_budget` (units: KJ/hour/g)
#' #' @slot energy_expenditure numeric, total energy spent on all activities by the
#' #'   end of the current time-step (units: KJ/g)
#' #' @slot foraging_success numeric, the mass of prey consumed per time-step
#' #'   (units: g/time-step)
#' #' @slot mass_change_value numeric, specifying the change in the agent’s mass
#' #'   over the time-step (units: g)
#' #' @slot mortality_prob numeric, the probability of the agent dying within the
#' #'   the current time-step
#' #' @slot alive logical, indicating the agent's survival status (`TRUE` if
#' #'   alive, `FALSE` if dead)
#' #' @slot track object of class `sf`, containing time-location points that
#' #'   represent the agent's movement in the current time-step
#' #'
#' AgentState <- function(location = sf::st_point(),
#'                        grid_cell = sf::st_point(),
#'                        timestep = NA_integer_,
#'                        timestamp = as.Date(NA),
#'                        body_mass =  NA_real_,
#'                        activity_budget = list(
#'                          flight = NA,
#'                          water_active = NA,
#'                          water_inactive = NA,
#'                          nest_resting = NA,
#'                          diving = NA,
#'                          other = NA),
#'                        activity_costs = list(
#'                          flight = NA,
#'                          water_active = NA,
#'                          water_inactive = NA,
#'                          nest_resting = NA,
#'                          diving = NA),
#'                        mortality_prob =  NA_real_,
#'                        alive = NA,
#'                        track = sf::st_sf(
#'                          timestamp = as.POSIXct(NA),
#'                          geom = sf::st_sfc(sf::st_point())
#'                        )){
#'
#' }

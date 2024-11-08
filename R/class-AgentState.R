#' `<AgentState>`
#'
#' `AgentState` is an S4 class for specifying an agent's current state in terms
#' of spatio-temporal activity and physiological condition at the end of the
#' current simulation `time_step` (e.g. end of day). It stores information such
#' as body mass, activity budgets, energy expenditure metrics, survival status,
#' etc
#'
#' @slot location object of class `XY`, specifying the agent's current location
#'   (units/projection: TBD).
#' @slot grid_cell object of class `XY`, indicating the spatial coordinates of
#'   the grid-cell occupied by the agent
#' @slot timestep integer, the index of current simulation time-step
#' @slot date `Date` object, the calendar date of the current simulation time-step
#' @slot mass numeric, the agent's current body mass (units: g)
#' @slot activity_budget named list, defining the agent's time-in-activity budget in
#'   the current time-step. Each list element represents an activity and the
#'   time the agent spends on it during this time-step (units: hours/time-step).
#' @slot activity_costs named list, specifying the energy costs associated with each
#'   activity in argument `activity_budget` (units: KJ/hour/g)
#' @slot energy_expenditure numeric, total energy spent on all activities by the
#'   end of the current time-step (units: KJ/g)
#' @slot foraging_success numeric, the mass of prey consumed per time-step
#'   (units: g/time-step)
#' @slot mass_change_value numeric, specifying the change in the agent’s mass
#'   over the time-step (units: g)
#' @slot mortality_prob numeric, the probability of the agent dying within the
#'   the current time-step
#' @slot alive logical, indicating the agent's survival status (`TRUE` if
#'   alive, `FALSE` if dead)
#' @slot track object of class `sf`, containing time-location points that
#'   represent the agent's movement in the current time-step
#'
#' @export

methods::setClass(
  Class = "AgentState",
  slots = list(
    location = "XY",
    grid_cell = "XY",
    timestep = "integer",
    date = "Date",
    mass = "numeric",
    activity_budget = "list",
    activity_costs = "list",
    energy_expenditure = "numeric",
    foraging_success = "numeric",
    mass_change_value = "numeric",
    mortality_prob = "numeric",
    alive = "logical",
    track = "sf"
  ),
  prototype = list(
    location = sf::st_point(),
    grid_cell = sf::st_point(),
    timestep = NA_integer_,
    date = as.Date(NA),
    mass =  NA_real_,
    activity_budget = list(
      flight = NA,
      water_active = NA,
      water_inactive = NA,
      nest_resting = NA,
      diving = NA,
      other = NA
    ),
    activity_costs = list(
      flight = NA,
      water_active = NA,
      water_inactive = NA,
      nest_resting = NA,
      diving = NA
    ),
    mortality_prob =  NA_real_,
    alive = NA,
    track = sf::st_sf(
      timestamp = as.POSIXct(NA),
      geom = sf::st_sfc(sf::st_point())
    )
  )
)

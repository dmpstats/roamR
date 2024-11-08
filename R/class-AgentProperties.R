#' `<AgentProperties>`
#'
#' `<AgentProperties>` is an S4 class specifying the individual-level properties
#' of a given agent being traced in the simulation model. It subsumes features
#' that remain constant throughout simulation run, including biological and
#' physical traits, movement properties, initialization and and final states,
#' susceptibility to impact effects,
#'
#' @param species_id character, the identifier code for the agent's species
#' @param initial_mass numeric, the agent's body mass at the start of the
#'   simulation (g)
#' @param wingspan numeric, the agent's maximum extent across its wings (units: cm)
#' @param age numeric, the age of the agent at the start of the simulation
#' @param sex character, the sex of the agent, where `"f"`denotes female and
#'   `"m"` denotes male
#' @param speed named list, defining the travel speed properties for the agent. Each
#'   element specifies the distribution of speeds, in terms of mean and
#'   coefficient of variation (cv), for a given movement type  (e.g. flight
#'   speed and swim speed).
#' @param start_location,end_location objects of class `XY`, the location of the
#'   agent at the start and end of the simulation, respectively
#' @param mortality_tresh numeric, the threshold body mass below which the agent
#'   is assumed to die (units: g)
#' @param disturbance_efects named list, indicating how anthropogenic disturbances
#'   impact key properties of the agent. Each list element specifies
#'   the direction and magnitude of the effect on an affected property#'
#' @param redistributed logical, indicating the agent's susceptibility to change
#'   its expected spatial distribution in response to man-made structures (`TRUE`
#'   if susceptible, `FALSE` if not)
#'
#' @export
#'
methods::setClass(
  Class = "AgentProperties",
  slots = list(
    species_id = "character",
    initial_mass = "numeric",
    wingspan = "numeric",
    speed = "list",
    start_location = "XY",
    end_location = "XY",
    mortality_tresh = "numeric",
    disturbance = "list",
    redistributed = "logical",
    age = "numeric",
    sex = "character"
  ),
  prototype = list(
    species_id = NA_character_,
    initial_mass = NA_real_,
    wingspan = NA_real_,
    speed = list(
      flight = list(mean = NA_real_, cv = NA_real_),
      swim = list(mean = NA_real_, cv = NA_real_)
    ),
    start_location = sf::st_point(),
    end_location = sf::st_point(),
    mortality_tresh = NA_real_,
    disturbance = list(
      flight = list(dir = NA_character_, pctg = NA_real_),
      diving = list(dir = NA_character_, pctg = NA_real_),
      foraging_success = list(dir = NA_character_, pctg = NA_real_)
    ),
    redistributed = NA,
    age = NA_real_,
    sex = NA_character_
  )
)

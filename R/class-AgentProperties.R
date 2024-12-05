#' `<AgentProperties>`
#'
#' @description
#' `<AgentProperties>` is an S4 class specifying the individual-level properties
#' of a given agent being traced in the simulation model. It subsumes features
#' that remain constant throughout simulation run, including biological and
#' physical traits, movement properties, initialization and and final states,
#' susceptibility to impact effects,
#'
#' Currently this is an "internal" class, i.e. it is not meant to be user-facing
#'
#' @slot species_id character, the identifier code for the agent's species
#' @slot initial_mass numeric, the agent's body mass at the start of the
#'   simulation (g)
#' @slot wingspan numeric, the agent's maximum extent across its wings (units: cm)
#' @slot age numeric, the age of the agent at the start of the simulation
#' @slot sex character, the sex of the agent, where `"f"`denotes female and
#'   `"m"` denotes male
#' @slot speed named list, defining the travel speed properties for the agent. Each
#'   element specifies the distribution of speeds, in terms of mean and
#'   coefficient of variation (cv), for a given movement type  (e.g. flight
#'   speed and swim speed).
#' @slot start_location,end_location objects of class `XY`, the location of the
#'   agent at the start and end of the simulation, respectively
#' @slot mortality_tresh numeric, the threshold body mass below which the agent
#'   is assumed to die (units: g)
#' @slot disturbance_efects named list, indicating how anthropogenic disturbances
#'   impact key properties of the agent. Each list element specifies
#'   the direction and magnitude of the effect on an affected property#'
#' @slot redistributed logical, indicating the agent's susceptibility to change
#'   its expected spatial distribution in response to man-made structures (`TRUE`
#'   if susceptible, `FALSE` if not)
#'
#' @include s4_management.R class-VarDist.R s4_utils.R utils.R
#'
#'
#' @export
#'
methods::setClass(
  Class = "AgentProperties",
  slots = list(
    species_id = "character",
    initial_mass = "units",
    wingspan = "units",
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
    initial_mass = units::set_units(NA, "g"),
    wingspan = units::set_units(NA, "cm"),
    speed = list(
      flight = VarDist(),
      swim = VarDist()
    ),
    start_location = sf::st_point(),
    end_location = sf::st_point(),
    mortality_tresh = NA_real_,
    disturbance = list(
      flight = VarDist(),
      diving = VarDist(),
      foraging_success = VarDist()
    ),
    redistributed = NA,
    age = NA_real_,
    sex = NA_character_
  )
)

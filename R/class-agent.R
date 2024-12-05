#' `<Agent>`
#'
#' `<Agent>` is an S4 class representing the biological properties and
#' historical state of an individual agent within the simulation model. It uses
#' classes <[AgentProperties-class]> and <[AgentState-class]>, collating a
#' historical record of the agent's movement and state properties generated
#' throughout the simulation.
#'
#' @slot state object of class <[AgentState-class]>, describing the agent's
#'   state for the current simulation time-step.
#' @slot properties object of class <[AgentProperties-class]>, defining the
#'   individual-level properties of the agent
#' @slot history object of class `sf`, storing historical data comprising the
#'   agent's state and spatio-temporal activity for each time-step of the
#'   simulation.
#'
#' @include class-AgentState.R class-AgentProperties.R
#'
#' @seealso
#'    * Helper function [Agent()] to construct `<Agent>` objects
#'    * Helper functions [AgentState()] and [AgentProperties()]
#'
#' @export

methods::setClass(
  Class = "Agent",
  slots = list(
    state = "AgentState",
    properties = "AgentProperties",
    history = "sf"
  ),
  prototype = list(
    state = new("AgentState"),
    properties = new("AgentProperties"),
    history = sf::st_sf(
      timestamp = as.POSIXct(NA),
      geom = sf::st_sfc(sf::st_point()),
      body_mass = NA,
      foraging = NA,
      energy = NA,
      alive = NA
    )
  )
)




#' Create `<Agent>` objects
#'
#' `Agent()` is a helper function to construct instances of `[Agent-class]`
#' objects
#'
#' @param state object of class [AgentState-class], describing the agent's
#'   state for the current simulation time-step.
#' @param properties object of class [AgentProperties-class], defining the
#'   individual-level properties of the agent
#' @param history object of class `<sf>`, storing historical data comprising the
#'   agent's state and spatio-temporal activity for each time-step of the
#'   simulation.
#'
#' @seealso
#'    * Helper functions [AgentState()] and [AgentProperties()]
#'
#' @export
Agent <- function(state = new("AgentState"),
                  properties = new("AgentProperties"),
                  history = sf::st_sf(
                    timestamp = as.POSIXct(NA),
                    geom = sf::st_sfc(sf::st_point()),
                    body_mass = NA,
                    foraging = NA,
                    energy = NA,
                    alive = NA)){

  # Handling NA inputs --------------
  if(all(is.na(history))){
    history <- sf::st_sf(
      timestamp = as.POSIXct(NA),
      geom = sf::st_sfc(sf::st_point()),
      body_mass = NA,
      alive = NA)
  }

  # Input validation ----------------------------
  check_class(state, "AgentState")
  check_class(properties, "AgentProperties")
  check_class(history, "sf")


  # construct a new instance of <Agent> ----------
  methods::new(
    Class = "Agent",
    state = state,
    properties = properties,
    history = history
  )



  # if(!is(species, "Species")){
  #   cli::cli_abort(c(
  #     "{.arg species} must be an object of class {.cls Species}",
  #     "x" = "You've supplied a {.cls {class(species)}} object",
  #     "i" = "Use {.code Species()} to generate a {.cls Species} object"
  #   ))
  # }
  #
  # if(!is.numeric(alive)){
  #   cli::cli_abort(
  #     "Argument {.arg alive} must be {.cls numeric}, not {.cls {class(alive)}}"
  #     )
  # }else if(alive %notin% c(NA_integer_, 0, 1)){
  #   cli::cli_abort(
  #     "{.arg alive} must be either 0 or 1, not `{.val {alive}}`")
  # }
  #
  #
  # if(!is.numeric(body_mass)){
  #   cli::cli_abort(
  #     "Argument {.code body_mass} must be {.cls numeric}, not {.cls {class(body_mass)}}")
  # }



}









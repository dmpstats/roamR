#' `<Agent>`
#'
#' `<Agent>` is an S4 class representing the biological properties and dynamic
#' state of an individual agent within the simulation model. It inherits from S4
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




#' #' Create `<Agent>` objects
#' #'
#' #' `Agent()` is a helper function to construct instances of `<Agent>` S4 objects with user-defined values
#' #'
#' #' @param location object of class `XY`, specifying the current location of
#' #'   the agent in terms of (x,y) coordinates
#' #' @param timestamp object of class `POSIXct`, providing the timestamp at the current
#' #'   location
#' #' @param body_mass numeric, the current body weight of the agent (units: kg)
#' #' @param alive binary, specifying whether the agent is currently dead (`0`) or alive (`1`)
#' #' @param history object of class `sf`, to store historical data of the agent
#' #'   throughout the simulation
#' #' @param species an object of class [Species-class], specifying the
#' #'   species-level properties of the agent. Use [Species()] to generate the expected object
#' #'
#' #' @include class-Species.R
#' #'
#' #' @export
#' Agent <- function(
#'     timestamp = as.POSIXct(NA),
#'     location = NA,
#'     body_mass = NA,
#'     alive = NA,
#'     species = NA,
#'     age = NA,
#'     sex = NA,
#'     history = NA){
#'
#'
#'   # Handling NULL and NA inputs --------------
#'   if(is.na(history)){
#'     history <- sf::st_sf(timestamp = as.POSIXct(NA), geom = sf::st_sfc(sf::st_point()))
#'   }
#'
#'   if(is.na(location)){
#'     location <- sf::st_point()
#'   }
#'
#'   if(is.na(species)){
#'     species = new("Species")
#'   }
#'
#'   # Ensure NA types are correct
#'   if(is.na(alive)) alive <- NA_integer_
#'   if(is.na(bodyWt)) bodyWt <- NA_real_
#'
#'
#'   # input validation ----------------------------
#'   if(!is(species, "Species")){
#'     cli::cli_abort(c(
#'       "{.arg species} must be an object of class {.cls Species}",
#'       "x" = "You've supplied a {.cls {class(species)}} object",
#'       "i" = "Use {.code Species()} to generate a {.cls Species} object"
#'     ))
#'   }
#'
#'   if(!is.numeric(alive)){
#'     cli::cli_abort(
#'       "Argument {.arg alive} must be {.cls numeric}, not {.cls {class(alive)}}"
#'       )
#'   }else if(alive %notin% c(NA_integer_, 0, 1)){
#'     cli::cli_abort(
#'       "{.arg alive} must be either 0 or 1, not `{.val {alive}}`")
#'   }
#'
#'
#'   if(!is.numeric(body_mass)){
#'     cli::cli_abort(
#'       "Argument {.code body_mass} must be {.cls numeric}, not {.cls {class(body_mass)}}")
#'   }
#'
#'
#'   # Defaults for NULL arguments --------------
#'   if(is.null(history)){
#'     history <- sf::st_sf(timestamp = as.POSIXct(NA), geom = sf::st_sfc(sf::st_point()))
#'   }
#'
#'   if(is.null(location)){
#'     location <- sf::st_point()
#'   }
#'
#'
#'   # construct a new instance of <Agent> ----------
#'   methods::new(
#'     Class = "Agent",
#'     location = location,
#'     timestamp = timestamp,
#'     species = species,
#'     condition = list(
#'       body_mass = body_mass,
#'       alive = alive
#'     ),
#'     history = history
#'   )
#' }









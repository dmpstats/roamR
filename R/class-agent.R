#' Class `'Agent'`
#'
#' `'Agent'` is an S4 class specifying properties of the individual agent being
#' traced in the simulation model. It subsumes the underlying specifics of the
#' species as well as general features such as current location, time,
#' condition. In addition, the class allows for the storage of historical data
#' (movement and other properties of interest)
#'
#' @include class-species.R
#'
#'
#' @slot currentLocation object of class `XY`, specifying the current location of
#'   the agent in terms of (x,y) coordinates
#' @slot currentTime object of class `POSIXlt`, providing the timestamp at the current
#'   location
#' @slot type character string, the name of the class representing the type of agent.
#'   Defaults to the S4 class [Species-class]
#' @slot condition a `list` object containing the current condition of the agent.
#'   Currently is support the following named elements:
#'   * bodyWt: numeric value, the current body weight of the agent (units: kg)
#'   * alive: binary, specifying whether the agent is dead (`0`) or alive (`1`)
#'
#' @slot history object of class `sf`, to store historical data of the agent
#'   throughout the simulation
#' @export
setClass(
  "Agent",
  slots = list(
    currentLocation = "XY",
    currentTime = "POSIXlt",
    type = "Species",
    condition = "list",
    history = "sf"
  ),
  prototype = list(
    currentLocation = sf::st_point(),
    currentTime = as.POSIXlt(NA),
    type = new("Species"),
    condition = list(
      bodyWt = units::set_units(NA, "kg"),
      alive = NA_integer_
    ),
    history = sf::st_sf(timestamp = as.POSIXlt(NA), geom = sf::st_sfc(sf::st_point()))
  )
)


#' Class `<DriverResponse>`
#'
#' `<DriverResponse>` is an S4 class that defines species-level, per-agent
#' responses to a specific environmental or man-made "driver", as declared in an
#' associated object of class <[`Driver-class`]>.
#'
#' Designed to be a sub-class of <[`Species-class`]>, `<DriverResponse>` is
#' tightly integrated with class <[`Driver-class`]> to enable the specification
#' of influencing factors that directly drive the movement, behaviour activities
#' and/or the condition of the simulated agents.
#'
#'
#' For model initialization, a `<DriverResponse>` object must be linked to its
#' corresponding <[`Driver-class`]> object via the `@driver_id` slot in
#' `<DriverResponse>` and the `@id` slot in <[`Driver-class`]>. Unmatched IDs
#' will result in an error during the IBM's initialization phase.
#'
#'
#' @slot driver_id character string, the unique identifier of the driver. It
#'   must match the `@id` value in the concomitant <[`Driver-class`]> object.
#'   Unmatched IDs will trigger an error during model initialization.
#' @slot movement an object of class <[`MoveInfluence-class`]>, specifying how
#'   the driver influences the agent's movement
#' @slot states a list of <[`StateInfluence-class`]> objects, listing the
#'   influence of the driver in the agent's states.
#' @slot condition a character string (feature under development). Reserved for
#'   specifying additional effects of the driver on agent condition.
#'
#' @seealso Helper function [DriverResponse()] for constructing
#'   `<DriverResponse>` objects
#'
#' @include class-MoveInfluence.R
#'
#' @export

methods::setClass(
  Class = "DriverResponse",
  slots = list(
    driver_id = "character",
    movement = "MoveInfluence",
    states = "list",
    condition = "character"
  ),
  prototype = list(
    driver_id = NA_character_,
    movement = new("MoveInfluence"),
    states = list(),
    condition = NA_character_
  )
)




#' Create a `<DriverResponse>` object
#'
#' Helper function to construct instances of <[`DriverResponse-class`]> objects,
#' defining per-agent responses to a specific environmental or man-made
#' "driver", as declared in an associated object of class <[`Driver-class`]>.
#'
#' For model initialization, a `<DriverResponse>` object must be linked to its
#' corresponding <[`Driver-class`]> object via the `@driver_id` slot in
#' `<DriverResponse>` and the `@id` slot in <[`Driver-class`]>. Unmatched IDs
#' will result in an error during the IBM's initialization phase.
#'
#' @param driver_id character string, the unique identifier of the driver. It
#'   must match the `@id` value in the concomitant <[`Driver-class`]> object.
#'   Unmatched IDs will trigger an error during model initialization.
#' @param movement an object of class <[`MoveInfluence-class`]>, specifying how the
#'   driver influences the agent's movement
#' @param states a list of <[`StateInfluence-class`]> objects, listing the
#'   influence of the driver in the agent's states.
#' @param condition a character string (feature under development). Reserved for
#'   specifying additional effects of the driver on agent condition.
#'
#' @export

DriverResponse <- function(driver_id = NA_character_,
                           movement = MoveInfluence(),
                           states = list(),
                           condition = NA_character_){

  # allow for specification of unique, unlisted, <StateInfluence> objects
  # to `states`, if of the correct class
  if(is(states, "StateInfluence")) states <- list(states)

  # input validation
  check_class(movement, "MoveInfluence", class_fn = "roamR::MoveInfluence")
  if(length(states) > 0) check_class(states, "StateInfluence", inlist = TRUE)

  # construct a new instance of <DriverResponse>
  new(
    "DriverResponse",
    driver_id = driver_id,
    movement = movement,
    states = states,
    condition = condition
  )

}

#' Class `<BehaviourSpec>`
#'
#' An S4 class containing the specifications for a single animal behaviour. The
#' intention is to insert objects of this type into a list of behaviours pf
#' interest, making the IBM more flexible generalizable.
#'
#' @include class-VarDistr.R
#'
#' @export

methods::setClass(
  Class = "BehaviourSpec",
  slots = list(
    id = "character",
    energy_cost = "VarDistr",
    time_budget = "VarDistr",
    speed = "VarDistr"
  ),
  prototype = list(
    id = NA_character_,
    energy_cost = VarDistr(NA_real_, NA_real_),
    time_budget = VarDistr(NA_real_, NA_real_),
    speed = VarDistr(NA_real_, NA_real_)
  )
)

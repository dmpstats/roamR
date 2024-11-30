#' Class `<BehaviourSpec>`
#'
#' An S4 class containing the specifications for a single animal behaviour. The
#' intention is to insert objects of this type into a list of behaviours of
#' interest, making the IBM more flexible and generalizable.
#'
#' @include class-VarDist.R s4_management.R s4_utils.R utils.R
#'
#' @export

methods::setClass(
  Class = "BehaviourSpec",
  slots = list(
    behav = "character",
    energy_cost = "VarDist",
    time_budget = "VarDist",
    speed = "VarDist"
  ),
  prototype = list(
    behav = NA_character_,
    energy_cost = VarDist(),
    time_budget = VarDist(),
    speed = VarDist()
  )
)



# BehaviourSpec <- function(behav = NA_character_,
#                           energy_cost = VarDist(),
#                           time_budget = dist_missing(),
#                           speed = dist_missing()){
#
#
#   new(
#     "BehaviourSpec",
#     behav = behav,
#     energy_cost = energy_cost,
#     time_budget = time_budget,
#     speed = speed
#   )
#
# }

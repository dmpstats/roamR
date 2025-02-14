#' `<StateInfluence>`
#'
#' @include utils.R s4_utils.R class-VarDist.R
#'
#' @export
#'
methods::setClass(
  Class = "StateInfluence",
  slots = list(
    state_id = "character",
    prob = "VarDist",
    extent = "VarDist"
  )
)



#' @export
StateInfluence <- function(state_id = NA_character_,
                           prob = VarDist(),
                           extent = VarDist()){

  # TOOO
  # - check on valid distributions for `prob`, i.e. dist_binom or dist_degenerate
  # - add valid behaviours


  new(
    "StateInfluence",
    state_id = state_id,
    prob = prob,
    extent = extent
  )

}

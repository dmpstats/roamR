#' `<ActivInfluence>`
#'
#' @include utils.R s4_utils.R class-VarDist.R
#'
methods::setClass(
  Class = "ActivInfluence",
  slots = list(
    behav = "character",
    prob = "VarDist",
    extent = "VarDist"
  )
)



#' @export
ActivInfluence <- function(behav = NA_character_,
                           prob = VarDist(),
                           extent = VarDist()){

  # TOOO
  # - check on valid distributions for `prob`, i.e. dist_binom or dist_degenerate
  # - add valid behaviours


  new(
    "ActivInfluence",
    behav = behav,
    prob = prob,
    extent = extent
  )

}

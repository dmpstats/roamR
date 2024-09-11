#' Class `'Species'`
#'
#' `'Species'` is an S4 class specifying species-specific properties of the
#' simulated agents e.g. the properties that will influence the agents movements
#' (e.g. speed), condition (e.g. initial body-weight), behavioural properties
#' (e.g. diving, day-light), parameters for condition changes etc.
#'
#' @slot species character string, the common name of the species
#' @slot speedDist list object, defining probability distribution of flight
#'   speed. List comprises the following named elements:
#'    - `dist`: character string, the name of the probability distribution
#'    - `par1`: numeric, distribution parameter 1
#'    - `par2`: numeric, distribution parameter 2
#' @slot sizeDist list, defining the probability distribution of body-weight.
#'   List comprises the following named elements:
#'    - `dist`: character string, the name of the probability distribution
#'    - `par1`: numeric, distribution parameter 1
#'    - `par2`: numeric, distribution parameter 2
#'
#' @export
setClass(
  "Species",
  slots = list(
    species = "character",
    speedDist = "list",
    sizeDist = "list"
  ),
  prototype = list(
    species = NA_character_,
    speedDist = list(dist = "normal", par1 = 0, par2 = 1),
    sizeDist = list(dist = "normal", par1 = 0, par2 = 1)
  )
)

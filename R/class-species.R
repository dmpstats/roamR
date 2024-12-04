#' `<Species>`
#'
#' `<Species>` is an S4 class that comprises species-level properties of the
#' simulated agents. It includes attributes that dictate individual agents'
#' biological features, movement patterns, behavioural decisions, and responses
#' to anthropogenic and other non-environmental impacts.
#'
#'
#' @slot id character string, the identifier code for the species
#' @slot common_name character string, the common name of the species
#' @slot scientific_name character string, the scientific name of the species
#' @slot role character string, defining the role of the species in the IBM (one
#'   of "agent", "prey", "competitor")
#' @slot mass_distr an object of class [VarDist-class], defining the species'
#'   distribution of body mass
#' @slot spatial_distr a `<stars>` array, comprising a time-series of
#'   raster-type density surfaces of the species covering the area of interest.
#'   First 2 dimensions are expected to provide the spatial properties of the
#'   density surfaces. The 3rd dimension specifies the temporal resolution of
#'   the data, while the 4th dimension relates to draws (e.g. bootstrap samples)
#'   of the density surfaces
#' @slot behaviour_profile a list comprising objects of class
#'   [BehaviourSpec-class], defining the behavioural profile of the species.
#' @slot impact_responses a list containing objects of class
#'   [ImpactResponse-class], specifying per-agent responses to impact factors.
#' @slot mortality_thresh_distr an object of class [VarDist-class], specifying
#'   the values of agent's condition (e.g. body mass), below which the agent is
#'   assumed to have died
#'
#' @include class-VarDist.R class-ImpactResponse.R
#'
#' @seealso
#'  * Helper function [Species()] to define `<Species>` objects
#'  * Helper functions [VarDist()], [ImpactResponse()] and [BehaviourSpec()] for constructing
#'   objects of the dependency classes
#'
#' @export

methods::setClass(
  "Species",
  slots = list(
    id = "character",
    role = "character",
    common_name = "character",
    scientific_name = "character",
    mass_distr = "VarDist",
    spatial_distr = "stars",
    behaviour_profile = "list",
    impact_responses = "list",
    mortality_thresh_distr = "VarDist"
  ),
  prototype = list(
    id = NA_character_,
    role = NA_character_,
    common_name = NA_character_,
    scientific_name = NA_character_,
    spatial_distr = stars::st_as_stars(matrix(NA)),
    mass_distr = VarDist(),
    mortality_thresh_distr = VarDist(),
    behaviour_profile = list(
      flying = BehaviourSpec(behav = "flying"),
      swimming = BehaviourSpec(behav = "swimming"),
      diving = BehaviourSpec(behav = "diving"),
      foraging =  BehaviourSpec(behav = "foraging"),
      water_resting = BehaviourSpec(behav = "water_resting"),
      nest_attending = BehaviourSpec(behav = "nest_attending"),
      other = BehaviourSpec(behav = "other")
    ),
    impact_responses = list(
      impact_1 = ImpactResponse(impact_id = "owf_footprint"),
      impact_2 = ImpactResponse(impact_id = "owf_buffer")
    )
  )
)




#' Create `<Species>` objects
#'
#' Helper function to construct instances of [Species-class] objects, which
#' comprise species-level properties of the simulated agents. It includes
#' attributes that dictate individual agents' biological features, movement
#' patterns, behavioural decisions, and responses to anthropogenic and other
#' non-environmental impacts.
#'
#'
#' @param id character string, the identifier code for the species
#' @param common_name character string, the common name of the species
#' @param scientific_name character string, the scientific name of the species
#' @param role character string, defining the role of the species in the IBM (one
#'   of "agent", "prey", "competitor")
#' @param mass_distr an object of class [VarDist-class], defining the species'
#'   distribution of body mass
#' @param spatial_distr a `<stars>` array, comprising a time-series of
#'   raster-type density surfaces of the species covering the area of interest.
#'   First 2 dimensions are expected to provide the spatial properties of the
#'   density surfaces. The 3rd dimension specifies the temporal resolution of
#'   the data, while the 4th dimension relates to draws (e.g. bootstrap samples)
#'   of the density surfaces
#' @param behaviour_profile a list comprising objects of class
#'   [BehaviourSpec-class], defining the behavioural profile of the species.
#' @param impact_responses a list containing objects of class
#'   [ImpactResponse-class], specifying per-agent responses to impact factors.
#' @param mortality_thresh_distr an object of class [VarDist-class], specifying
#'   the values of agent's condition (e.g. body mass), below which the agent is
#'   assumed to have died
#'
#' @seealso
#'  * Helper functions [VarDist()], [ImpactResponse()] and [BehaviourSpec()]
#'  * [stars::st_as_stars()] for generating `<stars>` arrays
#'
#'
Species <- function(id = NA_character_,
                    role = c("agent", "prey", "competitor"),
                    common_name = NA_character_,
                    scientific_name = NA_character_,
                    mass_distr = VarDist(),
                    mortality_thresh_distr = VarDist(),
                    spatial_distr = stars::st_as_stars(matrix(NA)),
                    behaviour_profile = list(),
                    impact_responses = list()){

  # input validation
  role <- rlang::arg_match(role)

  check_class(id, "character")
  check_class(role, "character")
  check_class(common_name, "character")
  check_class(mass_distr, "VarDist")
  check_class(mortality_thresh_distr, "VarDist")
  check_class(spatial_distr, "stars")
  if(length(behaviour_profile) > 0) check_class(behaviour_profile, "BehaviourSpec", inlist = TRUE)
  if(length(impact_responses) > 0) check_class(impact_responses, "ImpactResponse", inlist = TRUE)


  # construct a new instance of <Species>
  new(
    "Species",
    role = role,
    common_name = common_name,
    scientific_name = scientific_name,
    mass_distr = mass_distr,
    mortality_thresh_distr = mortality_thresh_distr,
    spatial_distr = spatial_distr,
    behaviour_profile = behaviour_profile,
    impact_responses = impact_responses
  )


}




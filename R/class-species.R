#' `<Species>`
#'
#' `<Species>` is an S4 class specifying species-level properties of the
#' simulated agents e.g. the properties that will dictate agent-level attributes
#' and movement features
#'
#' @slot species_id character string, the identifier code for the species
#' @slot common_name character string, the common name of the species
#' @slot scientific_name character string, the scientific name of the species
#' @slot role character string, defining the role of the species in the IBM (one
#'   of "agent", "prey", "competitor")
#' @slot speed_distr named list, defining the species-level travel speed attributes.
#'   Each element specifies the distribution of speeds for a given movement type
#'   (e.g. flight speed and swim speed) in terms of in terms of mean (`m`) and
#'   coefficient of variation (`cv`).
#' @slot mass_distr named list, defining the species' distribution of body mass
#'   in terms of mean (`m`) and CV `cv`
#' @slot spatial_distr a `stars` array, comprising a time-series of grid-type
#'   density surfaces of the species covering the area of interest. First 2
#'   dimensions are expected to provide the spatial properties of the density
#'   surfaces. The 3rd dimension contains specifies the temporal resolution of
#'   the data, while the 4th dimension relates to draws (e.g. bootstrap samples)
#'   of the density surfaces
#' @slot activity_budget_distr named list, defining the distribution of
#'   activity budgets of the species. TODO: expand
#' @slot energy_cost_distr named list, defining the distribution of
#'   energy costs of the species. TODO: expand
#' @slot mortality_thresh_distr named list, specifying the range of possible
#'   values of agent's condition (e.g. body mass), below which the agent is
#'   assumed to have died
#' @slot redistribution_type a function
#' @slot redistr_prob a list
#' @slot disturb_prob a list
#'
#' #' @include class-VarDistr.R
#'
#' @export

methods::setClass(
  "Species",
  slots = list(
    id = "character",
    role = "character",
    common_name = "character",
    scientific_name = "character",
    spatial_distr = "stars",
    mass_distr = "VarDist",
    behaviour_profile = "list",
    # speed_distr = "list",
    # activity_budget_distr = "list",
    # energy_cost_distr = "list",
    mortality_thresh_distr = "list",
    redistribution_type = "function",
    redistr_prob = "list",
    disturb_prob = "list"
  ),
  prototype = list(
    id = NA_character_,
    role = NA_character_,
    common_name = NA_character_,
    scientific_name = NA_character_,
    mass_distr = VarDist(),
    spatial_distr = stars::st_as_stars(matrix(NA)),
    behaviour_profile = list(
      flying = new(
        "BehaviourSpec",
        behav = "flight",
        speed = VarDist(),
        energy_cost = VarDist(),
        time_budget = VarDist()
      ),
      swimming = new(
        "BehaviourSpec",
        behav = "swim",
        speed = VarDist(),
        energy_cost = VarDist(),
        time_budget =  VarDist()
      )
    ),
    # speed_distr = list(
    #   flight = list(m = NA_real_, cv = NA_real_),
    #   swim = list(m = NA_real_, cv = NA_real_)
    # ),
    # activity_budget_distr = list(
    #   flight = list(m = NA_real_, cv = NA_real_),
    #   sea_active = list(m = NA_real_, cv = NA_real_),
    #   sea_resting = list(m = NA_real_, cv = NA_real_),
    #   nest_resting = list(m = NA_real_, cv = NA_real_),
    #   diving = list(m = NA_real_, cv = NA_real_),
    #   other = list(m = NA_real_, cv = NA_real_)
    # ),
    # energy_cost_distr = list(
    #   flight = list(m = NA_real_, cv = NA_real_),
    #   sea_active = list(m = NA_real_, cv = NA_real_),
    #   sea_resting = list(m = NA_real_, cv = NA_real_),
    #   nest_resting = list(m = NA_real_, cv = NA_real_),
    #   diving = list(m = NA_real_, cv = NA_real_),
    #   other = list(m = NA_real_, cv = NA_real_)
    # ),
    mortality_thresh_distr = list(m = NA_real_, cv = NA_real_),
    redistribution_type = function(){},
    redistr_prob = list(
      footprint = list(m = NA_real_, cv = NA_real_),
      buffer = list(m = NA_real_, cv = NA_real_)
    ),
    disturb_prob = list(
      footprint = list(m = NA_real_, cv = NA_real_),
      buffer = list(m = NA_real_, cv = NA_real_)
    )
  )
)


#' #' Create `<Species>` objects
#' #'
#' #' Helper function to construct instances of `<Species>` S4 objects
#' #'
#' Species <- function(id ,
#'                     role,
#'                     common_name = "character",
#'                     scientific_name = "character",
#'                     spatial_distr = "stars",
#'                     mass_distr = "VarDistr",
#'                     behaviour_profile = "list",
#'                     mortality_thresh_distr = "list",
#'                     redistribution_type = "function",
#'                     redistr_prob = "list",
#'                     disturb_prob = "list"){
#'
#' }





#' #' @param common_name character string, the common name of the species
#' #' @param scientific_name character string, the scientific name of the species
#' #' @param type character string, designating the type of agent (e.g. `'bird'`,
#' #'   `'fish'`, `'dragon'`)
#' #' @param speed_dist list defining the probability distribution of flight
#' #'   speed, comprising the following elements:
#' #'    - `dist`: character string, the name of the probability distribution
#' #'    - `par1`: numeric, distribution parameter 1
#' #'    - `par2`: numeric, distribution parameter 2
#' #' @slot size_dist list defining the probability distribution of body-weight,
#' #'   comprising the following elements:
#' #'    - `dist`: character string, the name of the probability distribution
#' #'    - `par1`: numeric, distribution parameter 1
#' #'    - `par2`: numeric, distribution parameter 2
#' #'
#' #'
#' #' @export
#' Species <- function(common_name,
#'                     scientific_name = NA,
#'                     speed_dist = list(dist = NA, par1 = NA, par2 = NA),
#'                     size_dist = list(dist = NA, par1 = NA, par2 = NA)){
#'
#'
#'   # ensuring correct type
#'   common_name <- as.character(common_name)
#'
#'   # handling NA inputs
#'   if(is.na(scientific_name)) scientific_name <- NA_character_
#'
#'
#'   # construct a new instance of <Species> ----------
#'   methods::new(
#'     "Species",
#'     common_name = common_name,
#'     scientific_name = scientific_name,
#'     speed_dist = speed_dist,
#'     size_dist = size_dist
#'   )
#'
#' }





#' #' Validator for <Species>
#' #'
#' setValidity("Species", function(object){
#'
#'   errors <- character()
#'
#'   # @speedDist
#'
#'   dst_le <- c("dist", "par1", "par2")
#'
#'   #missing_elements <- union()
#'
#'   if("dist" %notin% names(object@speedDist)){
#'     msg <- "The list underpinning slot @speedDist must contain element 'dist'"
#'
#'     errors <- c(errors, msg)
#'   }
#'
#'
#'   if(length(errors) == 0){
#'     TRUE
#'   } else {
#'     errors
#'   }
#'
#' })

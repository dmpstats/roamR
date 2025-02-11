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
#' @slot body_mass_distr an object of class [VarDist-class], defining the species'
#'   distribution of body mass
#' @slot spatial_distr a `<stars>` array, comprising a time-series of
#'   raster-type density surfaces of the species covering the area of interest.
#'   First 2 dimensions are expected to provide the spatial properties of the
#'   density surfaces. The 3rd dimension specifies the temporal resolution of
#'   the data, while the 4th dimension relates to draws (e.g. bootstrap samples)
#'   of the density surfaces
#' @slot pop_size numeric, the population size within the Area of calculation
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
    spatial_distr = "stars",
    pop_size = "numeric",
    body_mass_distr = "VarDist",
    behaviour_profile = "list",
    impact_responses = "list",
    driver_responses = "list",
    mortality_thresh_distr = "VarDist"
  ),
  prototype = list(
    id = NA_character_,
    role = NA_character_,
    common_name = NA_character_,
    scientific_name = NA_character_,
    spatial_distr = stars::st_as_stars(matrix(NA)),
    pop_size = NA_real_,
    body_mass_distr = VarDist(),
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
    ),
    driver_responses = list(
      land = new("DriverResponse", driver_id = "land"),
      sst = new("DriverResponse", driver_id = "sst")
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
#' @param body_mass_distr an object of class [VarDist-class], defining the species'
#'   distribution of body mass
#' @param spatial_distr a `<stars>` array, comprising a time-series of
#'   raster-type density surfaces of the species covering the area of interest.
#'   First 2 dimensions are expected to provide the spatial properties of the
#'   density surfaces. The 3rd dimension specifies the temporal resolution of
#'   the data, while the 4th dimension relates to draws (e.g. bootstrap samples)
#'   of the density surfaces
#' @param pop_size numeric, the population size within the Area of calculation
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
#' @examples
#'
#' library(distributional)
#'
#' # 1. Start by setting the behaviour profile
#' behav <- list(
#'  flight = BehaviourSpec(
#'     behav = "flying",
#'     energy_cost = VarDist(dist_uniform(1, 2), units = "kJ/hour/gram"),
#'     time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
#'     speed = VarDist(dist_uniform(10, 20), "m/s")
#'     ),
#'  dive = BehaviourSpec(
#'     behav = "dive",
#'     energy_cost = VarDist(dist_uniform(1, 2), units = "kJ/hour/gram"),
#'     time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
#'     speed = VarDist(dist_uniform(10, 20), "m/s")
#'   )
#' )
#'
#' # 2. the list of impact responses
#' imp_resp <- list(
#'   owf_footprint = ImpactResponse(
#'     impact_id = "owf_foot",
#'     displace_prob = VarDist(dist_beta(2, 8)),
#'     displace_fn = function(x, y){ x/y},
#'     disturb_prob = VarDist(dist_beta(1, 5)),
#'     disturb_behav = "flying",
#'     # extra hours/day of flight due to impact
#'     disturb_extent = VarDist(dist_lognormal(2, 1), "hr/day")
#'   )
#' )
#'
#' # 3. `stars` array with 10 samples of monthly density distribution
#' dens <- data.frame(
#'            expand.grid(x= 1:5, y = 1:5, month = 3:5, iter = as.integer(1:10)),
#'            counts = rlnorm(5*5*3*10)) |>
#'         st_as_stars(dims = c("x", "y", "month", "iter"))
#'
#' # 4. specify <Species>
#' guill <- Species(
#'      id = "guill",
#'      role = "agent",
#'      common_name = "guillemot",
#'      scientific_name = "Uria Aalge",
#'      body_mass_distr = VarDist(dist_uniform(850, 1130), "g"),
#'      mortality_thresh_distr = VarDist(dist_normal(500, 20), "g"),
#'      spatial_distr = dens,
#'      behaviour_profile = behav,
#'      impact_responses = imp_resp
#'    )
#'
Species <- function(id = NA_character_,
                    role = c("agent", "prey", "competitor"),
                    common_name = NA_character_,
                    scientific_name = NA_character_,
                    body_mass_distr = VarDist(),
                    mortality_thresh_distr = VarDist(),
                    spatial_distr = stars::st_as_stars(matrix(NA)),
                    pop_size = NA_real_,
                    behaviour_profile = list(),
                    impact_responses = list(),
                    driver_responses = list()){

  # allow for unlisted objects assigned to `behaviour_profile` and `impact_responses`, if of the correct class
  if(is(behaviour_profile, "BehaviourSpec")) behaviour_profile <- list(behaviour_profile)
  if(is(impact_responses, "ImpactResponse")) impact_responses <- list(impact_responses)
  if(is(driver_responses, "DriverResponse")) driver_responses <- list(driver_responses)

  # input validation ---------------
  role <- rlang::arg_match(role)

  check_class(id, "character")
  check_class(role, "character")
  check_class(common_name, "character")
  check_class(body_mass_distr, "VarDist")
  check_class(mortality_thresh_distr, "VarDist")
  check_class(spatial_distr, "stars")
  if(length(behaviour_profile) > 0) check_class(behaviour_profile, "BehaviourSpec", inlist = TRUE)
  if(length(impact_responses) > 0) check_class(impact_responses, "ImpactResponse", inlist = TRUE)
  if(length(driver_responses) > 0) check_class(driver_responses, "DriverResponse", inlist = TRUE)


  # TODO:
  #   (i) sense check on chosen distributions and parameters given the nature of
  #       the variable. Implied args: body_mass_distr, mortality_thresh_distr
  #   (ii) ensure behaviours are nor duplicated


  # construct a new instance of <Species>
  new(
    "Species",
    role = role,
    common_name = common_name,
    scientific_name = scientific_name,
    body_mass_distr = body_mass_distr,
    mortality_thresh_distr = mortality_thresh_distr,
    spatial_distr = spatial_distr,
    pop_size = pop_size,
    behaviour_profile = behaviour_profile,
    impact_responses = impact_responses,
    driver_responses = driver_responses
  )
}


# TODO
#  - implement `show` method for <Species> for neat display

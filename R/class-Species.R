#' `<Species>`
#'
#' `<Species>` is an S4 class that comprises species-level properties of
#' simulated agents. It includes attributes related to taxonomy, physiology,
#' behavioural/activity states, responses to environmental and non-environmental
#' drivers, and population size.
#'
#' @slot id character string, a unique identifier for the species.
#' @slot common_name character string, the common name of the species.
#' @slot scientific_name character string, the scientific name of the species.
#' @slot body_mass_distr an object of class [VarDist-class], defining the
#'   distribution of body mass for the species.
#' @slot energy_to_mass_distr a [VarDist-class] object, providing the
#'   energy-to-bodymass conversion rate for the species (e.g. g/kJ). Determines
#'   how gained or lost energy translates into changes in body mass.
#' @slot states_profile a list of [State-class] objects, defining the
#'   behavioural/activity states for the species to consider in the model.
#' @slot driver_responses list of [DriverResponse-class] objects, specifying the
#'   species’ responses to predefined model drivers, such as environmental
#'   pressures, biological influences, and human-induced impacts affecting
#'   movement and behaviour/activity states. See [DriverResponse-class] for
#'   details.
#'
#'   Each element in the `driver_responses` list must correspond to an existing
#'   object of class [Driver-class]. At the initialization phase of the
#'   Individual-Based Model (IBM), these `driver_responses` are matched with
#'   their respective [Driver-class] objects to ensure a consistent and
#'   well-defined set of species-driver interactions. Failure to provide a
#'   corresponding [Driver-class] object for each entry of `driver_responses`
#'   will result in missing or undefined driver effects during the simulation.
#'
#' @slot mortality_thresh_distr an object of class [VarDist-class], specifying
#'   the values of agent's condition (e.g. body mass), below which it is
#'   assumed to have died (*Note: slot may require further generalization*)
#' @slot pop_size numeric, the population size (i.e., number of individuals)
#'   within the area of calculation (AOC).
#'
#' @include class-VarDist.R
#'
#' @seealso
#'  * Helper function [Species()] to define `<Species>` objects
#'  * Helper functions [VarDist()], [DriverResponse()] and [State()] for
#'  constructing objects of the dependency classes
#'
#' @export

methods::setClass(
  "Species",
  slots = list(
    id = "character",
    common_name = "character",
    scientific_name = "character",
    body_mass_distr = "VarDist",
    energy_to_mass_distr = "VarDist", # (g/Kj)
    states_profile = "list",
    driver_responses = "list",
    mortality_thresh_distr = "VarDist",
    pop_size = "numeric"
    #energy_intake = "VarFn"
  ),
  prototype = list(
    id = NA_character_,
    common_name = NA_character_,
    scientific_name = NA_character_,
    pop_size = NA_real_,
    body_mass_distr = VarDist(),
    mortality_thresh_distr = VarDist(),
    states_profile = list(),
    driver_responses = list()
  )
)




#' Create `<Species>` objects
#'
#' Helper function to construct instances of [Species-class] objects, representing
#' species-level properties of simulated agents. These include attributes
#' related to taxonomy, physiology, behavioural/activity states, responses to
#' environmental and non-environmental drivers, and population size.
#'
#'
#' @param id character string, a unique identifier for the species.
#' @param common_name character string, the common name of the species.
#' @param scientific_name character string, the scientific name of the species.
#' @param body_mass_distr an object of class [VarDist-class], defining the
#'   distribution of body mass for the species.
#' @param energy_to_mass_distr a [VarDist-class] object, providing the
#'   energy-to-bodymass conversion rate for the species (e.g. g/kJ). Determines
#'   how gained or lost energy translates into changes in body mass.
#' @param states_profile a list of [State-class] objects, defining the
#'   behavioural/activity states for the species to consider in the model.
#' @param driver_responses a list of [DriverResponse-class] objects, specifying
#'   the species’ responses to predefined model drivers, such as environmental
#'   pressures, biological influences, and human-induced impacts affecting
#'   movement and behaviour/activity states. See the Details section below and
#'   [DriverResponse-class] for further information.
#'
#' @param mortality_thresh_distr an object of class [VarDist-class], specifying
#'   the values of agent's condition (e.g. body mass), below which it is
#'   assumed to have died (*Note: parameter may require further generalization*)
#' @param pop_size numeric, the population size (i.e., number of individuals)
#'   within the area of calculation (AOC).
#'
#'
#' @details
#' Each element in the `driver_responses` list must correspond to an existing
#' object of class [Driver-class]. At the initialization phase of `{roamR}`s
#' IBM, these `driver_responses` are matched with their respective
#' [Driver-class] objects to ensure a consistent set of species-driver
#' interactions. Failure to provide a corresponding [Driver-class] object for
#' each entry of `driver_responses` will result in missing or undefined driver
#' effects during the simulation.
#'
#'
#' @seealso
#' Helper functions [VarDist()], [DriverResponse()] and [State()]
#'
#' @export
#'
#' @examples
#'
#' library(distributional)
#'
#' # 1. Start by setting the state profile
#' states <- list(
#'  flight = State(
#'     id = "flying",
#'     energy_cost = VarDist(dist_uniform(1, 2), units = "kJ/hour/gram"),
#'     time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
#'     speed = VarDist(dist_uniform(10, 20), "m/s")
#'     ),
#'  dive = State(
#'     id = "diving",
#'     energy_cost = VarDist(dist_uniform(1, 2), units = "kJ/hour/gram"),
#'     time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
#'     speed = VarDist(dist_uniform(10, 20), "m/s")
#'   )
#' )
#'
#' # 2. Then the list of driver responses
#' driver_resp <- list(
#'   coast = DriverResponse(
#'     driver_id = "land",
#'     movement = MoveInfluence(
#'       prob = VarDist(distributional::dist_degenerate(1)),
#'       fn = function(x, slope = 1/2) exp(-slope * x),
#'       type = "repulsion"
#'     )),
#'   owf = DriverResponse(
#'     driver_id = "owf_foot",
#'     movement = MoveInfluence(
#'       prob = VarDist(dist_beta(2, 8)),
#'       fn = function(x, slope = 1/2) exp(-slope * x),
#'       type = "repulsion"
#'     ),
#'     states = list(
#'       StateInfluence(
#'         state_id = "flying",
#'         prob = VarDist(dist_beta(1, 5)),
#'         extent = VarDist(dist_lognormal(2, 1), "hr/day")
#'       )))
#'   )
#'
#' # 3. specify <Species>
#' guill <- Species(
#'      id = "guill",
#'      common_name = "guillemot",
#'      scientific_name = "Uria Aalge",
#'      pop_size = 10000,
#'      body_mass_distr = VarDist(dist_uniform(850, 1130), "g"),
#'      mortality_thresh_distr = VarDist(dist_normal(500, 20), "g"),
#'      states_profile = states,
#'      driver_responses = driver_resp
#'    )
#'
Species <- function(id = NA_character_,
                    common_name = NA_character_,
                    scientific_name = NA_character_,
                    body_mass_distr = VarDist(),
                    energy_to_mass_distr = VarDist(),
                    mortality_thresh_distr = VarDist(),
                    states_profile = list(),
                    driver_responses = list(),
                    pop_size = NA_real_){

  # allow for unlisted objects assigned to `states_profile` and `impact_responses`, if of the correct class
  if(is(states_profile, "State")) states_profile <- list(states_profile)
  if(is(driver_responses, "DriverResponse")) driver_responses <- list(driver_responses)

  # input validation ---------------

  check_class(id, "character")
  check_class(common_name, "character")
  check_class(body_mass_distr, "VarDist")
  check_class(energy_to_mass_distr, "VarDist")
  check_class(mortality_thresh_distr, "VarDist")
  if(length(states_profile) > 0) check_class(states_profile, "State", inlist = TRUE)
  if(length(driver_responses) > 0) check_class(driver_responses, "DriverResponse", inlist = TRUE)


  # TODO:
  #   (i) sense check on chosen distributions and parameters given the nature of
  #       the variable. Implied args: body_mass_distr, mortality_thresh_distr
  #   (ii) ensure behaviours are not duplicated


  # construct a new instance of <Species>
  new(
    "Species",
    id = id,
    common_name = common_name,
    scientific_name = scientific_name,
    body_mass_distr = body_mass_distr,
    energy_to_mass_distr = energy_to_mass_distr,
    mortality_thresh_distr = mortality_thresh_distr,
    pop_size = pop_size,
    states_profile = states_profile,
    driver_responses = driver_responses
  )
}



# Validator -----------------------------------------------------
methods::setValidity("Species", function(object) {
  errors <- character()

  # states_profile - state@time_budgets checks: units must be convertible across states
  if( length(object@states_profile) > 1){
    unts <- sapply(object@states_profile, \(state) state@time_budget@units)
    non_conv <- combn(unts, 2, \(x) !units::ud_are_convertible(x[[1]], x[[2]]))
    if (any(non_conv)) {
      msg <- cli::format_inline(
        "\n - slot @states_profile: units of {.cls State} slot @time_budget must ",
        "be convertible across the specified states. {.val {unts}} are not convertible units."
      )
      errors <- c(errors, msg)
    }
  }

  if(length(errors) == 0) TRUE else do.call(paste, list(errors, collapse = " "))
})





# Methods -----------------------------------------------------

## utils  ----

#' @include s4_utils.R
methods::setMethod("is_empty", "Species", function(object){
  #length(object@driver_responses) == 0
  length(object@states_profile) == 0
})








# TODO
# 1. `show` method for <Species> for neat display

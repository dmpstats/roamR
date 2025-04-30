#' `<Agent>`
#'
#' `<Agent>` is an S4 class encapsulating the biological and physiological
#' properties and historical status of an individual agent within the simulated
#' IBM. It uses classes <[AgentCondition-class]> and
#' <[AgentProperties-class]>, also collating a historical record of the agent's
#' movement and state properties generated throughout the simulation.
#'
#' @slot condition object of class <[AgentCondition-class]>, describing the
#'   agent's condition at the end of the current simulation time-step.
#' @slot properties object of class <[AgentProperties-class]>, defining
#'   attributes of the agent that remain constant throughout simulation.
#' @slot history object of class `sf`, storing historical data comprising the
#'   agent's state and spatio-temporal activity for each time-step of the
#'   simulation.
#'
#' @include class-AgentCondition.R class-AgentProperties.R
#'
#' @seealso
#'    * Helper function [Agent()] to construct `<Agent>` objects
#'    * Helper functions [AgentProperties()] and [AgentCondition()]
#'
#' @export

methods::setClass(
  Class = "Agent",
  slots = list(
    condition = "AgentCondition",
    properties = "AgentProperties",
    history = "sf"
  ),
  prototype = list(
    condition = new("AgentCondition"),
    properties = new("AgentProperties"),
    history = sf::st_sf(sf::st_sfc())
  )
)


# TODO
# - Methods
#   1. A `show` method for <Agent> for neat display: a data frame?
#   2. A `plot` method


#' Create `<Agent>` objects
#'
#' `Agent()` is a helper function for constructing instances of [Agent-class]
#' objects. It relies on predefined classes `<Species>` and `<ModelConfig>`
#' objects to initiate its slots accordingly.
#'
#' @param species object of class <[Species-class]>, specifying the agent's
#'   species-level properties. If `NULL` (default), species-related slots in
#'   `<Agent>` are initialized as empty.
#' @param model_config object of class <[ModelConfig-class]>, defining the IBM's
#'   configuration. If `NULL` (default), model-related slots in `<Agent>` are
#'   initialized as empty.
#'
#' @seealso
#'    * Helper functions [Species()] and [ModelConfig()]
#'
#' @export
Agent <- function(species = NULL, model_config = NULL){

  # NULL input handling
  species <- species %||% Species()
  model_config <- model_config %||% ModelConfig()

  # Input validation ----------------------------
  check_class(species, "Species")
  check_class(model_config, "ModelConfig")


  if(is_empty(species)){

    condition <- new("AgentCondition")
    properties <- new("AgentProperties")
    history <- sf::st_sf(geom = sf::st_sfc())

  }else{

    # initialize <AgentProperties> -----------------------------------
    properties <- AgentProperties(species = species, model_config = model_config)

    # initialize <AgentCondition> -----------------------------------
    ## placeholding slots - i.e. currently ignored
    grid_cell <- sf::st_point()
    mortality_prob <- NA_real_

    ## State Budgets: initialize budgets (relative time lengths) and standardise
    ## i.e. to be treated as probabilities, adding up to 1
    state_budget <- lapply(species@states_profile, \(s) generate(s@time_budget))
    budget_sum <- Reduce("+", state_budget)
    state_prob <- lapply(state_budget, \(b) b/budget_sum)

    # Starting at 0 cost
    states_cost <- lapply(species@states_profile, \(state) units::set_units(0, kJ/g/h))

    condition <- new(
      "AgentCondition",
      location = properties@start_point,
      grid_cell = grid_cell,
      timestep = 0L,
      timestamp = as.POSIXct(model_config@start_date, "UTC"),
      body_mass = properties@initial_mass,
      states_budget = state_prob,
      states_cost = states_cost,
      energy_expenditure = units::set_units(0, "kJ/g"),
      foraging_success = units::set_units(0, "g/day"),
      mass_change_value = units::set_units(0, "g"),
      mortality_prob = mortality_prob,
      alive = TRUE,
      track = sf::st_sf(
        timestamp = as.POSIXct(NA),
        geom = sf::st_sfc(sf::st_point())
      ))


    # Populate @history ---------------------------------------
    history <- sf::st_sf( # tibble::tibble(
      timestep = condition@timestep,
      body_mass = condition@body_mass,
      states_budget = list(condition@states_budget),
      energy_expenditure = condition@energy_expenditure,
      geometry = sf::st_sfc(condition@location)
    )

  }

  # construct a new instance of <Agent> ---------------------------------------
  methods::new(
    Class = "Agent",
    condition = condition,
    properties = properties,
    history = history
  )

}







# Methods -----------------------------------------------------


## Accessors ----

### @body_mass
setMethod("body_mass", "Agent", function(x) x@condition@body_mass)
setMethod("body_mass<-", "Agent", function(x, value) {
  x@condition@body_mass <- value
  validObject(x@condition)
  x
})


### @location
setMethod("location", "Agent", function(x) x@condition@location)
setMethod("location<-", "Agent", function(x, value) {
  x@condition@location <- value
  validObject(x@condition)
  x
})




# ## show ----
# setMethod("show", "Agent", function(object) {
#
#   cat(class(object), "instance with the following history\n\n")
#
#   print.data.frame(object@history)
#
#   # cat(is(object)[[1]], "\n",
#   #     "  Name: ", object@condition, "\n",
#   #     "  Age:  ", object@age, "\n",
#   #     sep = ""
#   # )
# })

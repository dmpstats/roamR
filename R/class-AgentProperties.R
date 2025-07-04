#' `<AgentProperties>`
#'
#' @description
#' `<AgentProperties>` is an S4 class specifying the individual-level properties
#' of an agent being traced in the simulation model. It subsumes features that
#' remain constant throughout the simulation run, including biological traits,
#' average travelling speeds, initialization and final locations,
#' susceptibility of movement and states to model drivers.
#'
#' Currently this is an "internal" class, i.e. not meant to be user-facing.
#'
#' @slot species_id character, the identifier code for the agent's species.
#' @slot initial_mass `<units>` object, the agent's body mass at the start of the
#'   simulation.
#' @slot speeds a named list, defining movement speed properties for the agent.
#'   Each element specifies the agent's average speed for a given movement state
#'   (e.g. flying, swimming, etc). List elements must be of type `<units>`.
#' @slot cost_par_draws a named list bla
#' @slot start_point,end_point objects of class `XY`, the spatial coordinates
#'   of the agent at the start and end of the simulation, respectively.
#'   the agent is assumed to die.
#' @slot mortality_thresh `<units>` object, the threshold body mass below which
#'   the agent is assumed to die.
#' @slot energy_to_mass a `<units>` object, providing the agent's
#'   energy-to-bodymass conversion rate (e.g. g/kJ).
#' @slot move_influences a named list, defining whether the agent is influenced
#'   by model drivers. Each element corresponds to a `driver_id` (which must be
#'   defined in the model's [Driver-class] object) and contains a single-row
#'   `data.frame` with columns:
#'    * `p`: numeric, providing the probability that the agent is influenced by
#'   the named driver.
#'    * `infl`: logical, whether the agent is influenced by the driver. It should
#'    be determined via a Bernoulli trial with probability `p`.
#' @slot state_influences a named list, defining whether agent states are
#'   influenced by model drivers. Each element, named after a `driver_id`,
#'   contains a `data.frame` with columns:
#'    * `state_id`: character string, the unique identifier of an agent's state.
#'    * `p`: numeric, the probability that the agent's `state_id` is influenced
#'    by the driver.
#'    * `infl`: logical, whether the agent's `state_id` is influenced by the
#'    driver. It should be determined via a Bernoulli trial with probability `p`.
#'    * `ext`: `<units>` value, the extent of the influence on the agent's `state_id`.
#'
#' @slot age `<units>` object, the age of the agent at the start of the
#'   simulation (currently unused).
#' @slot sex character, the sex of the agent, where `"f"`denotes female and
#'   `"m"` denotes male (currently unused).
#'
#' @seealso
#' Helper function [AgentProperties()] to create `<AgentProperties>` objects
#'
#' @include s4_management.R class-VarDist.R s4_utils.R utils.R
#'
#' @export
#'
methods::setClass(
  Class = "AgentProperties",
  slots = list(
    species_id = "character",
    initial_mass = "units",
    speeds = "list",
    #costings = "list",
    cost_par_draws = "list",
    start_point = "XY",
    end_point = "XY",
    mortality_thresh = "units",
    energy_to_mass = "units",
    move_influences = "list", #"data.frame",
    state_influences = "list",
    age = "units",
    sex = "character"
  ),
  prototype = list(
    species_id = NA_character_,
    initial_mass = units::set_units(NA, "g"),
    speeds = list(),
    #costings = list(),
    cost_par_draws = list(),
    start_point = sf::st_point(),
    end_point = sf::st_point(),
    mortality_thresh = units::set_units(NA, "g"),
    energy_to_mass = units::set_units(NA, "g/kJ"),
    move_influences = list(),
    state_influences = list(),
    age = units::set_units(NA, ""),
    sex = NA_character_
  )
)


#' Create `<AgentProperties>` objects
#'
#' `AgentProperties()` is a helper function to construct instances of
#' [AgentProperties-class] objects. This function initializes an agent's
#' properties, including body mass, movement parameters, and interactions with
#' environmental drivers.
#'
#' @param species_id character, the identifier code for the agent's species.
#' @param initial_mass `<units>` object, the agent's body mass at the start of the
#'   simulation.
#' @param speeds a named list, defining movement speed properties for the agent.
#'   Each element specifies the agent's average speed for a given movement state
#'   (e.g. flying, swimming, etc). List elements must be of type `<units>`.
#' @param cost_par_draws a named list bla
#' @param start_point,end_point objects of class `XY`, the spatial coordinates
#'   of the agent at the start and end of the simulation, respectively.
#' @param mortality_thresh `<units>` object, the threshold body mass below which
#'   the agent is assumed to die.
#' @param energy_to_mass a `<units>` object, providing the agent's
#'   energy-to-bodymass conversion rate (e.g. g/kJ).
#' @param move_influences a named list, defining whether the agent is influenced
#'   by model drivers. Each element corresponds to a `driver_id` (which must be
#'   defined in the model's [Driver-class] object) and contains a single-row
#'   `data.frame` with columns:
#'    * `p`: numeric, providing the probability that the agent is influenced by
#'   the named driver.
#'    * `infl`: logical, whether the agent is influenced by the driver. It should
#'    be determined via a Bernoulli trial with probability `p`.
#' @param state_influences a named list, defining whether agent states are
#'   influenced by model drivers. Each element, named after a `driver_id`,
#'   contains a `data.frame` with columns:
#'    * `state_id`: character string, the unique identifier of an agent's state.
#'    * `p`: numeric, the probability that the agent's `state_id` is influenced
#'    by the driver.
#'    * `infl`: logical, whether the agent's `state_id` is influenced by the
#'    driver. It should be determined via a Bernoulli trial with probability `p`.
#'    * `ext`: `<units>` value, the extent of the influence on the agent's `state_id`.
#' @param age `<units>` object, the age of the agent at the start of the
#'   simulation (currently unused).
#' @param sex character, the sex of the agent, where `"f"`denotes female and
#'   `"m"` denotes male (currently unused).
#' @param ... reserved for future extensions (currently ignored).
#' @param species a <[Species-class]> object, If not `NULL` (default), it is
#'   used to populate the slots of the returned `<AgentProperties>` object,
#'   overriding the above function arguments.
#' @param model_config  a [ModelConfig-class] object, used alongside Species to
#'   populate the returned `<AgentProperties>` object.
#'
#' @details
#'
#' If `species` (and `model_config`) are specified, all other arguments are
#' ignored. The [AgentProperties-class] object is then populated using the
#' provided `<Species>` and `<ModelConfig>` objects. Specifically:
#'
#' * `@initial_mass` is sampled from `Species@body_mass_distr`
#' * `@speeds` are generated based on `Species@behaviour_profile`
#' * `@mortality_thresh` is sampled from `Species@mortality_thresh_distr`
#' * `@start_point` and `@end_point` are defined from `ModelConfig@start_sites`
#' and `ModelConfig@end_sites`. In cases where these slots are empty, the
#' agent's start/end locations are randomly assigned within the AOC area based
#' on slot `ModelConfig@aoc_bbox`.
#'  * `@move_influences` and `state_influences` are generated from
#' `Species@driver_responses`.
#'
#' @return an object of class [AgentProperties-class]
#'
#' @export

AgentProperties <- function(species_id = NA_character_,
                            initial_mass = NULL,
                            speeds = list(),
                            #costings = list(),
                            cost_par_draws = list(),
                            start_point = sf::st_point(),
                            end_point = sf::st_point(),
                            mortality_thresh = NULL,
                            energy_to_mass = NULL,
                            move_influences = list(),
                            state_influences = list(),
                            age = NULL,
                            sex = c("f", "m"),
                            ...,
                            species = NULL,
                            model_config = NULL){

  if (is.null(species) || is_empty(species)) {

    # NULL handling
    initial_mass <- initial_mass %||% units::set_units(NA, "g")
    mortality_thresh <- mortality_thresh %||% units::set_units(NA, "g")
    energy_to_mass <- energy_to_mass %||% units::set_units(NA, "g/kJ")
    age <- age %||% units::set_units(NA, "") # currently ignored
    sex <- NA_character_  # currently ignored

  } else{

    if( is.null(model_config) ){
      cli::cli_abort("{.arg model_config} must be provided when {.arg species} is provided")
    }

    # input checking
    check_class(species, "Species")
    check_class(model_config, "ModelConfig")

    # get relevant IDs
    species_id <- species@id
    state_ids <- lapply(species@states_profile, \(s) s@id) |> unlist()

    # Sample values for randomly distributed variables ----------------------

    ## NOTE: These are assumed to be drawn from species-level distributions. As
    ## it stands, values sampled here are kept constant across the simulation.
    ## In other words, each agent gets one random draw of these quantities (e.g.
    ## flight speed) which remain unchanged throughout the simulation.

    initial_mass <- generate(species@body_mass_distr)
    mortality_thresh <- generate(species@mortality_thresh_distr)
    energy_to_mass <- generate(species@energy_to_mass_distr)

    # speeds
    speeds <- lapply(species@states_profile, \(s) generate(s@speed)) |>
      setNames(state_ids)
    #speeds[sapply(speeds, is.na)] <- NULL # drop states without speed specification

#     speeds <- lapply(species@states_profile, function(s){
#       #browser()
#       if(is(s@speed, "VarFn")){
#         # TODO - COMEBAK: probably needs a dedicated compiler function? or perhaps
#         # generalize the function compiler?
#         s@speed@cmp_f
#       }else{
#         generate(s@speed)
#       }
#     }) |>
#       setNames(state_ids)




    # # generate costing functions
    # costings <- purrr::map2(species@states_profile, state_ids, function(s, id){
    #   if(is(s@energy_cost, "VarFn")){
    #     #build_cost_fn(s@energy_cost)
    #     100
    #   } else{
    #     #browser()
    #
    #     # wraps the sampled value in a function, with the same arguments as the
    #     # function manufactured by build_cost_fn(), for streamlined calling
    #     # during simulation
    #     y <- generate(s@energy_cost)
    #
    #     # rlang::new_function(
    #     #   rlang::exprs(agent = NULL, drivers = NULL),
    #     #   rlang::expr(!!y)
    #     # )
    #
    #     #test_build(y)
    #     y
    #   }
    # }) |> setNames(state_ids)

    cost_par_draws <- purrr::map2(species@states_profile, state_ids, function(s, id){
      if(is(s@energy_cost, "VarFn")){
        #browser()

        lapply(s@energy_cost@args_spec, function(arg){
          if(arg@type == "random"){
            distributional::generate(arg@distr, 1)[[1]] |>
              units::set_units(arg@units, mode = "standard")
          }
        }) |> purrr::compact()

      } else{
        #browser()
        generate(s@energy_cost)
      }
    }) |> setNames(state_ids)


    # starting and ending points
    start_point <- get_point_in_site(model_config@start_sites, model_config@aoc_bbx)
    end_point <- get_point_in_site(model_config@end_sites, model_config@aoc_bbx)

    driver_ids <- sapply(species@driver_responses, \(resp) resp@driver_id)

    # generate drivers' influence on agent's movement
    move_influences <- lapply(species@driver_responses, function(resp){
      if( resp@movement@type != "null" ){
        # draw probability of agent's movement being influenced  by driver
        p <- generate(resp@movement@prob) |> as.numeric()
        # draw realization on whether agent's movement is influenced by driver
        infl <- distributional::dist_bernoulli(p) |>
          distributional::generate(1) |>
          unlist()
        #list(p = p, infl = infl)
        data.frame(p, infl)
      }else{
        NULL
      }
    }) |> setNames(driver_ids)


    # initialize drivers' influence on agent's states
    state_influences <- lapply(species@driver_responses, function(resp){
      lapply(resp@states, function(state){
        # sample probability of current state being influenced by current driver
        p <- generate(state@prob) |> as.numeric()
        # draw realization on whether status is influenced by driver and, if so,
        # sample the extent of the influence
        if( !is.na(p) ){
          infl <- distributional::dist_bernoulli(p) |>
            distributional::generate(1) |>
            unlist()
          ext <- if( infl ) generate(state@extent) else NA
        } else {
          infl <- ext <- NA
        }
        # collected results in data.frame
        data.frame(state_id = state@state_id, p, infl, ext)
      }) |> purrr::list_rbind()
    }) |> setNames(driver_ids)


    # currently non-processed elements, thus take the function's inputs, if specified
    age <- age %||% units::set_units(NA, "")
    sex <- NA_character_
  }


  # construct a new instance of <AgentProperties> --------------------------
  new(
    "AgentProperties",
    species_id = species_id,
    initial_mass = initial_mass,
    speeds = speeds,
    #costings = costings,
    cost_par_draws = cost_par_draws,
    start_point = start_point,
    end_point = end_point,
    mortality_thresh = mortality_thresh,
    energy_to_mass = energy_to_mass,
    move_influences = move_influences,
    state_influences = state_influences,
    age = age,
    sex = sex
  )

}


# test_build <- function(y){
#
#   rlang::new_function(
#     rlang::exprs(agent = NULL, drivers = NULL),
#     rlang::expr(!!y)
#   )
#
#   # function(agent = NULL, drivers = NULL){
#   #   eval(!!y)
#   # }
# }


# Validator -----------------------------------------------------
methods::setValidity("AgentProperties", function(object) {
  errors <- character()

  # speeds check
  if( length(object@speeds) > 0){
    if( is.null(names(object@speeds)) ){
      msg <- cli::format_inline("\n - slot @speeds must be a named {.cls list}" )
      errors <- c(errors, msg)
    }

    msg <- check_class(object@speeds, class = "units", inlist = TRUE,
                       return_msg = TRUE, arg = "@speeds")
    if(!is.null(msg)){
      errors <- c(errors, paste0("\n - ", msg) )
    }
  }

  # # costings check
  # if( length(object@costings) > 0){
  #   if( is.null(names(object@costings)) ){
  #     msg <- cli::format_inline("\n - slot @costings must be a named {.cls list}" )
  #     errors <- c(errors, msg)
  #   }
  #
  #   msg <- check_class(object@costings, class = "function", inlist = TRUE,
  #                      return_msg = TRUE, arg = "@costings")
  #   if(!is.null(msg)){
  #     errors <- c(errors, paste0("\n - ", msg) )
  #   }
  # }

  # move_influences
  if (length(object@move_influences) > 0) {
    if (is.null(names(object@move_influences))) {
      msg <- cli::format_inline("\n - slot @move_influences must be a named {.cls list}" )
      errors <- c(errors, msg)
    }
  }

  # state_influences
  if (length(object@state_influences) > 0) {
    if (is.null(names(object@state_influences))) {
      msg <- cli::format_inline("\n - slot @state_influences must be a named {.cls list}" )
      errors <- c(errors, msg)
    }
  }


  if(length(errors) == 0) TRUE else do.call(paste, list(errors, collapse = " "))
})




# Helpers ---------------------------------------------------------------------
get_point_in_site <- function(sites, aoc_bbox){
  sites_sfc <- sf::st_geometry(sites)
  if (length(sites_sfc) == 0) {
    return(sf::st_sample(aoc_bbox, 1)[[1]])
  }
  # randomly draw a site
  endpt <- sample(sites_sfc, 1, prob = sites$prop)
  # randomly draw a point inside geometry if site is not a point
  if (sf::st_geometry_type(endpt) != "POINT") {
    endpt <- sf::st_sample(endpt, 1)
  }
  endpt[[1]] # return sfg
}

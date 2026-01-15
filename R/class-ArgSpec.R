#' Class `<ArgSpec>`
#'
#' An S4 class for specifying the characteristics of a function argument.
#'
#' `<ArgSpec>` defines the properties of a function's argument, such as its
#' name, type, value or probability distribution, measurement units and general
#' description.
#'
#' At a lower level,`<ArgSpec>` interacts with the [`VarFn-class`] class to
#' define parameters for user-specified function that determine the value of a
#' model variable at each simulation iteration. At a higher level, it ensures
#' model robustness linking function arguments to the IBM's simulation
#' infrastructure.
#'
#'
#' @slot name character, the name of the argument.
#'
#' @slot type character, the expected type of argument within roamR's IBM
#'   context. Must be one of:
#'    - `"driver"`: An argument linked to an existing driver; its value is
#'    determined by the agent's status (e.g. location) at the current simulation
#'    step.
#'    - `"body_mass"`: Refers to the agent’s body mass at the current simulation step.
#'    - `"time_at_state"`: An argument related to time spent by the agent
#'   in a given state during the current simulation time step.
#'    - `"constant"`: The argument has a fixed value across simulations.
#'    - `"random"`: The argument is stochastic and it's value is dictated by a
#'    probability distribution.
#'
#' @slot value the fixed value assigned to the argument. Required only if `type
#'   = "constant"`.
#'
#' @slot driver_id character, required if `type = "driver"`; specifies the ID
#'   assigned to a given driver. This assumes the existence of a `<Driver>`
#'   object with a matching ID during the IBM's initialization phase (via
#'   `rmr_initiate()`). Failing that, initialization will not be successful.
#'
#' @slot state_id, character, required if `type = "time_at_state"`; specifies
#'   the ID of the referred state. Assumes the existence of a `<State>`
#'   object with a matching ID during the IBM's initialization phase (via
#'   `rmr_initiate()`).
#'
#' @slot description character string, a brief explanation of the argument's
#'   purpose.
#'
#' @slot distr Inherited from parent class <[`VarDist-class`]>, an object of
#'   class [`<distribution>`][distributional::distributional]. Required if `type
#'   = "random"`, representing the probability distribution associated with the
#'   argument's value.
#'
#' @slot units Inherited from parent class <[`VarDist-class`]>. A character
#'   string defining the measurement unit for the argument, either as a name
#'   (e.g. `"grams"`) or a symbol (e.g. `"m/s"`).  Units must be recognized by
#'   the [units::valid_udunits()] database.
#'
#'
#' @seealso
#' Helper function [ArgSpec()] to construct `<ArgSpec>` objects
#'
#' @include class-VarDist.R
#'
methods::setClass(
  Class = "ArgSpec",
  contains = "VarDist",
  slots = list(
    name = "character",
    type = "character",
    value = "ANY",
    driver_id = "character",
    state_id = "character",
    description = "character"
  ),
  prototype = list(
    name = NA_character_,
    type = NA_character_,
    value = NULL,
    driver_id = NA_character_,
    state_id = NA_character_,
    description = NA_character_
  )
)




#' Create a `<ArgSpec>` object
#'
#' Helper function to construct an instance of a <[`ArgSpec-class`]> object,
#' which defines the properties of a function's argument, including its name,
#' default value, probability distribution, measurement units, etc.
#'
#' @param name character, the name of the argument. If an empty string (`""`), an
#'   empty `<ArgSpec>` object is returned, regardless of other inputs.
#'
#' @param type character, the expected type of argument within roamR's IBM
#'   context. Must be one of:
#'    - `"driver"`: An argument linked to an existing driver; its value is
#'    determined by the agent's status (e.g. location) at the current simulation
#'    step.
#'    - `"body_mass"`: Refers to the agent’s body mass at the current simulation step.
#'    - `"time_at_state"`: An argument related to time spent by the agent
#'   in a given state during the current simulation time step.
#'    - `"constant"`: The argument has a fixed value across simulations.
#'    - `"random"`: The argument is stochastic, i.e. it's value is dictated by a
#'    probability distribution.
#'
#' @param distr a `<distributional>` object describing the probability
#'   distribution of the argument's values. Required only if `type = "random"`.
#'
#' @param value the fixed value assigned to the argument. Required only if `type
#'   = "constant".
#'
#' @param units character string defining the measurement unit for the argument
#'   within the function, either by name (e.g. `"grams"`) or abbreviation (e.g.
#'   `"m/s"`).  Units must be recognized by the [units::valid_udunits()]
#'   database. Defaults to:
#'  * "grams" if `type = "body_mass"`
#'  * "minutes" if `type = "time_at_state"`.
#'
#' @param driver_id character string, the ID of a driver associated with the argument
#'   (used when `type = "driver"`). This must match the ID of a `<Driver>` object
#'   available during model initialization via `rmr_initiate()`. If not defined,
#'   defaults to `name`. For all other `type`s, the associated slot `@driver_id` is
#'   set to `NA`.
#'
#' @param state_id, character, required if `type = "time_at_state"`; specifies
#'   the ID of the referred state. Assumes the existence of a `<State>`
#'   object with a matching ID during the IBM's initialization stage (via
#'   `rmr_initiate()`).
#'
#' @param description character string, a brief explanation of the argument's
#'   purpose.
#'
#'
#' @examples
#'
#' # driver ID set to `name` by default
#' ArgSpec("sst", "driver")
#'
#' # linking argument name to a given driver ID
#' ArgSpec("x", "driver", driver_id = "sst", units = "Degrees_celsius")
#'
#' # argument referring to agents' body mass, in kilograms
#' ArgSpec("b", "body_mass", units = "kg")
#'
#' # argument whose input values follow a Bernoulli distribution
#' ArgSpec("x", "random", distr = distributional::dist_bernoulli(0.1), units = "m")
#'
#'
#' @export
ArgSpec <- function(name,
                    type = c("driver", "body_mass", "time_at_state", "constant", "random"),
                    distr = NULL,
                    value = NULL,
                    units = NULL,
                    driver_id = NULL,
                    state_id = NULL,
                    description = NA_character_){


  # overwriting `name` shortcut ("") for returning empty instance of <VarFn> ---------------
  rlang::check_required(name)

  if(name == ""){
    return(new("ArgSpec"))
  }

  # input pre-processing -----------------------------------------------

  ## NULL handling
  units <- units %||% ""
  driver_id <- driver_id %||% NA_character_
  state_id <- state_id %||% NA_character_


  # input validation: 1st checks -----------------------------------------------

  #`name`
  check_class(name, "character")
  # `type`
  type <- rlang::arg_match(type)
  # `units`
  check_units(units)


  # Process argument specs based on `type` -------------------------------------

  ## "random" & `distr`
  if(type == "random"){
    if(is.null(distr)){
      cli::cli_abort("{.arg distr} must be provided when {.code type = \"random\"}.")
    }else{
     check_class(distr, "distribution")
    }
  } else{
    if(!is.null(distr)){
      cli::cli_warn("Value provided to {.arg distr} has been ignored, as it's only relevant when {.code type = \"random\"}.")
    }
    distr <- distributional::dist_missing() # forces missing distr if type is other than "random"
  }


  ## "constant" & `value`
  if(type == "constant"){
    if(is.null(value)) cli::cli_abort("{.arg value} must be provided when {.code type = \"constant\"}.")
  }

  ## Set default units for `type` "body_mass"
  if(type == "body_mass" && units == "") units <- "g"


  if(type == "time_at_state"){

    if(is.na(state_id)) cli::cli_abort("{.arg state_id} must be provided when {.code type = \"time_at_state\"}.")

    # default units for
    if(units == "") units <- "min"
  } else{
    # force state_id to NA for any other type
    state_id <- NA_character_
  }




  if(type == "driver"){
    ## Set default `driver_id` to `name`
    if(is.na(driver_id)) driver_id <- name
  } else{
    # or as NA for any other type
    driver_id <- NA_character_
  }


  ## construct instance of <ArgSpec> --------------
  new(
    "ArgSpec",
    name = name,
    type = type,
    distr = distr,
    value = value,
    units = units,
    driver_id = driver_id,
    state_id = state_id,
    description = description
  )

}





# Validator -----------------------------------------------------
methods::setValidity("ArgSpec", function(object) {

  err <- character()

  types <- c("driver", "body_mass", "time_at_state", "constant", "random")

  if(!is.na(object@type)){
    if(object@type %notin% types){
      msg <- cli::format_inline("\n - @type: must be one of {.val {vec_style(types)}}.")
      err <- c(err, msg)
    }

    if(object@type == "random"){
      if(is.na(object@distr)){
        msg <- cli::format_inline("\n - @distr must be provided when @type is {.val random}")
        err <- c(err, msg)
      }
    }

    if(object@type == "time_at_state"){
      if(is.na(object@state_id)){
        msg <- cli::format_inline("\n - @state_id must be provided when @type is {.val time_at_state}")
        err <- c(err, msg)
      }
    }
  }


  if(length(err) > 0){
    # need to collapse into single string for desired formatting
    do.call(paste, list(err, collapse = " "))
  } else{
    TRUE
  }

})



# Methods ------------------------------------------------------------

## Show ------------------------------------

setMethod("show", "ArgSpec", function(object) {

  cli::cat_line(c(
    cli::format_message("{.cls {class(object)}}"),
    format_argspec(object)
  ))

})



# utility for formatting <ArgSpec>
format_argspec <- function(object){

  # @name
  name_txt <- paste0("`", object@name, "`")

  # @units
  units_txt <- if(object@units == ""){
    ""
  } else{
    paste0(" [", units::deparse_unit(units::as_units(object@units)), "]") |>
      cli::col_grey()
  }

  spec_txt <- if(object@type == "constant"){
    object@value
  } else if(object@type == "random"){
    sub("\\[1\\] ", "", capture.output(object@distr)[2])
  } else if(object@type == "driver"){
    cli::format_message("driver {.val {object@driver_id}} evaluated at current step")
  } else if(object@type == "body_mass"){
    cli::format_message("agent's body mass at current step")
  } else if(object@type == "time_at_state"){
    cli::format_message("time spent on state {.val {object@state_id}} at current step")
  }

  sep_text <- ": "
  #sep_text <- ifelse(object@type == "random", " ~ ", " = ")

  argspec_txt <- paste0(name_txt, sep_text, spec_txt, units_txt)

  # @description
  descr_txt <- paste0("Description: ", object@description) |>
    cli::col_grey() |>
    cli::style_italic()

  if(!is.na(object@description)){
    argspec_txt <- paste0(argspec_txt, "\n", descr_txt)
  }

  argspec_txt
}

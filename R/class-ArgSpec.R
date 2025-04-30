#' Class `<ArgSpec>`
#'
#' `<ArgSpec>` is an S4 class designed to describe the specification of a
#' function argument, including its name, expected type, probability
#' distribution (if applicable) and a brief description.
#'
#' At a lower level,
#' `<ArgSpec>` interacts with the [`VarFn-class`] class to specify the input
#' arguments of a user-defined function. At a higher level, it contributes to
#' the broader **model definition**, helping to establish interactions between
#' the parent function and the model's infrastructure.
#'
#' @slot name character, the name of the argument.
#' @slot type character, the expected argument type within the `{roamR}` context.
#'  Must be one of:
#'    - `"driver"`,
#'    - `"body_mass"`,
#'    - `"time_at_state"`,
#'    - `"constant"`,
#'    - `"random"`
#' @slot value value
#' @slot driver_id character, required if `type = "driver"`.
#'
#' @slot description character string, a brief explanation of the argument's
#'   purpose.
#' @slot distr an object of class
#'   [`<distribution>`][distributional::distributional]. Required if `type` is
#'   set to `"random"`. Specifies the distribution of the argument's input,
#'   representing its expected value and uncertainty/variability.
#' @slot units a character string, defining the measurement units of the
#'   argument's input variable. Must be either a name (e.g. `"grams"`) or a symbol (e.g. `"m/s"`)
#'   that recognized is by the "udunits" database (see
#'   [units::valid_udunits()]).
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
    description = "character"
  ),
  prototype = list(
    name = NA_character_,
    type = NA_character_,
    value = NULL,
    driver_id = NA_character_,
    description = NA_character_
  )
)




#' Create a `<ArgSpec>` object
#'
#'
#' @param name character if an empty string `""` (value), an empty `<ArgSpec>` object
#'   is returned, regardless of the remaining inputs.
#'
#' @param distr a `<distributional>` object, specifying bla bla. Note: input is
#'   ignored if `type` is set to anything other than `"random"`.
#'
#' @param units blabla. Defaults to:
#'  * "grams" if `type == "body_mass"`
#'  * "minutes" if `type == "time_at_state"`
#'
#' @param driver_id blabla... it's forced as NA if `type` is anything other than "driver"
#'
ArgSpec <- function(name,
                    type = c("driver", "body_mass", "time_at_state", "constant", "random"),
                    distr = NULL,
                    value = NULL,
                    units = NULL,
                    driver_id = NULL,
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

  ## Set default units for `type` "body_mass" and "time_at_state"
  if(type == "body_mass" && units == "") units <- "g"
  if(type == "time_at_state" && units == "") units <- "min"


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


  }


  if(length(err) > 0){
    # need to collapse into single string for desired formatting
    do.call(paste, list(err, collapse = " "))
  } else{
    TRUE
  }

})





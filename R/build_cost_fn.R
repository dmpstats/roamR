#' State cost function factory
#'
#' Internal function operator for manufacturing functions to calculate energetic costs
#' associated with states based on a user-defined base function. This function
#' interprets the arguments of the base function, and works out the features to
#' construct a function that can be used in the model. The returned manufactured
#' function is a wrapper of the original (base) function that can be used within
#' the package infrastructure. It's arguments are fixed and provide the required
#' objects to compute or extract the input values for the original function.
#'
#' The function passed on to `base_fun` needs to observe a bunch of
#' requirements:
#'  * Argument names must be the ID of a driver, "b" for biomass and/or "t" for
#' time spent on activity on a current time-step
#'  * it is be responsible to handle the intended units
#'
#' @param vrf an `<VarFn>` object, the user-defined function for calculating the
#'   energy cost of a given state. It gets wrapped by a function that is usable
#'   within `{roamR}`s infrastructure.
#'
#' @importFrom rlang !!!
build_cost_fn <- function(vrf, state_id = NULL, step_duration = NULL){

  force(vrf)
  force(state_id)
  force(step_duration)


  # input validation
  check_class(vrf, "VarFn")

  # manufacture function for internal use
  #
  # manufactured function computes/extracts input values for each argument of
  # the user-defined function from the data inputs passed on to the manufactured
  # function
  function(agent, drivers, details = FALSE){

    check_class(agent, "Agent")
    check_class(drivers, "Driver", inlist = TRUE)

    arg_inputs <- purrr::imap(vrf@args_spec, function(arg, arg_nm){


      if(arg@type == "driver"){ # driver arg type


        val <- drivers |>
          purrr::keep(\(d) d@id == arg@driver_id) |>
          purrr::pluck(1) |>
          get_driver_cell_value(agent)

      }else{  # remaining arg types
        val <- switch(
          arg@type,
          body_mass = body_mass(agent),
          time_at_state = get_time_spent_at_state(agent, arg@state_id, arg@units, step_duration),
          constant = arg@value,
          random = get_drawn_cost_par(agent, state_id, arg_nm)
        )
      }

      # Perform required unit conversions. In practice, this is only
      # applicable to data-related arguments, such as `driver`, `body_mass` &
      # `time-at-state`, where discrepancies on unit scales might occur. Units
      # on other types of args are "self-contained".
      # Also implies that value takes the units from the fetched data by default
      # if the arg is specified as unitless (i.e. units = "")
      if(arg@units != "") units(val) <- arg@units

      # drop units for usage in user-defined function
      units(val) <- NULL
      val
    })

    # evaluates the base function by passing on the computed values
    out <- eval(rlang::expr(vrf@fn(!!!arg_inputs)))

    if(details) list(output = out, input = arg_inputs) else out
  }

}






# placeholder function to compute or extract time spent at state
get_time_spent_at_state <- function(agent, state_id, units, step_duration){

  if(is.null(step_duration)){
    cli::cli_abort("`step_duration` must be non-null")
  }

  x <- agent@condition@states_budget[[state_id]] * units::as_units(step_duration)

  units::set_units(x, units, mode = "standard")

}



# helper to get the cost parameter value that has been drawn for state_id
get_drawn_cost_par <- function(agent, state_id, arg_nm){

  state_ids <- names(agent@properties@cost_par_draws)

  if(state_id %notin% state_ids){
    cli::cli_abort("Can't find energy cost parameter for state ID {.val {state_id}}.")
  }

  agent@properties@cost_par_draws[[state_id]][[arg_nm]]
}







# build_cost_fn <- function(vrf, drivers, state_id = NULL){
#
#   # input validation
#   check_class(vrf, "VarFn")
#   check_class(drivers, "Driver", inlist = TRUE)
#
#   force(vrf)
#   force(drivers)
#
#   # get driver ids
#   driver_ids <- sapply(drivers, \(x) x@id)
#
#   # check consistency driver-based arguments and available drivers
#   driver_args <- purrr::keep(vrf@args_spec, \(x) x@type == "driver")
#
#   if(length(driver_args) > 0){
#
#     driver_args_names <- names(driver_args)
#     args_driver_ids <- sapply(driver_args, \(x) x@driver_id)
#     missing_drivers <- setdiff(args_driver_ids, driver_ids)
#
#     if(length(missing_drivers) > 0){
#       cli::cli_abort(paste0(
#         "{cli::qty(missing_drivers)} `driver_id`{?s} {.val {missing_drivers}} ",
#         "assigned to{?, respectively,} argument{?s} {cli::col_blue(driver_args_names)} ",
#         "{?doesn't/don't} match any of those present in {.arg drivers}."
#       ))
#       # cli::cli_abort(c(
#       #   "All driver-related arguments in {.arg vrf} must be specified in {.arg drivers}.",
#       #   x = "{cli::qty(missing_drivers)} `driver_id`{?s} {.val {missing_drivers}} associated with argument{?s} {cli::col_blue(driver_args_names)} {?doesn't/don't} match any of those in {.arg drivers}.",
#       #   i = "Possible {.arg driver_id}s: {.val {driver_ids}}."
#       # ))
#     }
#   }
#
#   # Sample one value for each random argument This means that the argument is
#   # sampled at initialization of each agent, remaining fixed for each agent
#   # across subsequent IBM simulation
#   args_spec(vrf) <- lapply(args_spec(vrf), function(arg){
#     if(arg@type == "random"){
#       arg@value <- distributional::generate(arg@distr, 1)[[1]]
#     }
#     arg
#   })
#
#
#   # manufacture function for internal use
#   #
#   # manufactured function computes/extracts input values for each argument of
#   # the base function from the data inputs passed on to the manufactured function
#   function(agent, drivers, details = FALSE){
#
#     check_class(agent, "Agent")
#     check_class(drivers, "Driver", inlist = TRUE)
#
#     arg_inputs <- lapply(vrf@args_spec, function(arg){
#
#       switch(
#         arg@type,
#         driver = get_driver_cell_value(drivers, arg@driver_id, agent),
#         body_mass = body_mass(agent) |> units::drop_units(),
#         time_at_state = get_time_spent_at_state(agent, state_id),
#         constant = arg@value,
#         random = arg@value # value drawn outside in function's factory environment
#       )
#     })
#
#     # evaluates the base function by passing on the computed values
#     out <- eval(rlang::expr(vrf@fn(!!!arg_inputs)))
#
#     if(details) list(output = out, input = arg_inputs) else out
#   }
#
# }


# build_cost_fn <- function(base_fn, drivers){
#
#   if(!is.function(base_fn)) cli::cli_abort("Argument {.arg base_fn} must be a function.")
#
#   force(base_fn)
#   force(drivers)
#
#   #browser()
#
#   bf_argnames <- rlang::fn_fmls_names(base_fn)
#   bf_argnames <- bf_argnames %||% ""
#
#
#   driver_ids <- sapply(drivers, \(x) x@id)
#
#   # any(bf_argnames %in% driver_ids)
#   # grepl("b|biom", bf_argnames)
#   # grepl("[state|activity|behaviour]_[time|duration]", bf_argnames)
#
#   # args of any output functions is fixed. What varies is the body of the output
#   # function based on characteristics of the input function
#   ff_args <- rlang::exprs(drivers = NULL, agent = NULL)
#
#   if(any(bf_argnames %in% driver_ids)){
#
#     ff_body <- rlang::expr({
#
#       driver_raster <- purrr::keep(drivers, \(d) d@id == !!bf_argnames)[[1]]@stars_obj
#       loc <- sf::st_coordinates(location(agent) + 1)
#       when <- "December" #agent_condition@timestamp
#
#       x <- stars::st_extract(
#         dplyr::filter(driver_raster, months == when),
#         at = loc
#       ) |> as.numeric()
#
#       .f <- !!base_fn
#       .f(x)
#     })
#
#   }else{
#     ff_body <- NULL
#   }
#
#   rlang::new_function(ff_args, ff_body)
# }




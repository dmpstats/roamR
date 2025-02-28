#' State cost function factory
#'
#' Function operator for manufacturing functions to calculate energetic costs
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
#' time spent on activity on a given time-step
#'  * it is be responsible to handle the intended units
#'
#' @param base_fun a function, the user-defined function for calculating the
#'   energy cost of a given state. It gets wrapped by a function that is usable
#'   within `{roamR}`s infrastructure.
#' @param drivers a list of `<Driver>` objects.
#'
#' @importFrom rlang !!!
build_cost_fn <- function(base_fn, drivers){

  if(!is.function(base_fn)) cli::cli_abort("Argument {.arg base_fn} must be a function.")

  force(base_fn)
  force(drivers)

  # Base function arg names
  bf_arg_names <- rlang::fn_fmls_names(base_fn)

  # get driver ids
  driver_ids <- sapply(drivers, \(x) x@id)

  # get types of arguments, driver, biomass ("b) or time-spent in state ("t")
  bf_arg_types <- sapply(bf_arg_names, function(arg){
    if(arg %in% driver_ids){
      type <- "driver"
    } else if (arg %in% c("b", "t")) {
      type <- arg
    } else{
      type <- "invalid"
    }
  })


  # invalid_args <- purrr::keep(bf_arg_types, \(x) x == "invalid")
  # if(length(invalid_args) > 0){
  #   cli::cli_abort(
  #     "Argument{?s} name{?s} {.val {names(invalid_args)}} in base function {?is/are} not valid.",
  #     class = "err-invalid-base-fn-arg"
  #   )
  # }


  # manufacture function
  function(drivers, agent){

    bf_arg_inputs <- purrr::map2(
      bf_arg_names, bf_arg_types,
      function(arg, type){
        switch(
          type,
          driver = get_driver_cell_value(drivers, arg, agent),
          b = body_mass(agent) |> as.numeric(),
          t = 333 #get_time_spent_at_state(agent)
        )
      })

    eval(rlang::expr(base_fn(!!!bf_arg_inputs)))
  }

}






get_driver_cell_value <- function(drivers, driver_id, agent){

  # TODO
  # - extract value of correct attribute (currently the complement of
  # c(aspect, slope), but maybe make it specific to driver_id instead, once that
  # is a requirement in the <Driver> definition?)
  # - proper filtering of the temporal dimension
  # - integrate potential presence of bootstrap replicates
  # - handle NAs returned from st_extract

  driver_stars <- drivers |>
    purrr::keep(\(d) d@id == driver_id) |>
    purrr::pluck(1) |>
    stars_obj() |>
    dplyr::select(!dplyr::any_of(c("slope", "aspect")))

  if(is_stars_empty(driver_stars)){
    cli::cli_abort(c(
      "No raster-type data available for driver {.val {driver_id}}",
      "x" = "Unable to extract values of {.val {driver_id}} to pass on to the base function"
    ))
  }else{
    loc <- sf::st_coordinates(location(agent) + 1)
    when <- "December" #agent_condition@timestamp
  }

  val <- stars::st_extract(
    dplyr::filter(driver_stars, months == when),
    at = loc
  )

  # garbage collection
  rm(driver_stars)

  val |> as.numeric()
}







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




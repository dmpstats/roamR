# the inverse of %in%
`%notin%` <- Negate(`%in%`)


# the inverse of is.null
not_null <- Negate(is.null)


#'
#' helper to check validity of probability distribution names
check_dist <- function(dist){

  dist <- match.arg(
    dist,
    choices = c("normal", "Normal",
                "poisson", "Poisson",
                "gamma", "Gamma",
                "binomial", "Binomial")
  )

  tolower(dist)
}




# units checker for character specification
check_units <- function(units_chr,
                        call = rlang::caller_env(),
                        arg = rlang::caller_arg(units_chr)){

  rlang::try_fetch(
    units::as_units(units_chr),
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      if(grepl("not recognized by udunits.", msg)){
        cli::cli_abort(
          c("{.str {units_chr}} is not a valid name or symbol for unit measurements",
            "x" = "{.arg {arg}} specification must comply with the udunits database (see {.fn units::valid_udunits})"),
          call = call,
          arg = arg,
          parent = NA,
          error = cnd,
          class = "err_units_string_misspec"
        )
      }}
  )

  # return nothing if check is passed
  invisible()
}



#' Check if units of an object are those expected under a specific context
#'
#' @param x, an object that inherits the class <units>
#' @param context character, the expected unit context of the object
#'
check_units_contextual <- function(x,
                                   context = c("length", "weight", "energy", "energy-time", "speed"),
                                   arg = rlang::caller_arg(x),
                                   call = rlang::caller_env()){

  stopifnot(inherits(x, "units"))

  context <- rlang::arg_match(context)

  if(context == "length"){
    ctx_units <- "meters"
    ctx_text <- "length"
  } else if(context == "weight"){
    ctx_units <- "g"
    ctx_text <- "weight"
  } else if(context == "energy"){
    ctx_units <- "kJ"
    ctx_text <- "energy"
  } else if(context == "energy-time"){
    ctx_units <- "kJ/hr"
    ctx_text <- "energy per unit-of-time"
  } else if(context == "speed"){
    ctx_units <- "km/hr"
    ctx_text <- "speed"
  }


  if(!units::ud_are_convertible(units(x), ctx_units)){

    cli::cli_abort(c(
      "Input values in {.arg {arg}} are expected to carry a valid unit of {ctx_text}.",
      x = "{.val {units::deparse_unit(x)}} is not a recognized {ctx_text} unit.",
      i = "Use e.g., {.val {ctx_units}} instead."
    ),
    call = call)
  }

  # return nothing if check is passed
  invisible()
}







#' Assertion for empty `<stars>` objects
#'
#' Currently `{stars}` doesn't appear to have a formalised way to define and
#' test empty objects. So, we re using `stars::st_as_stars(matrix(NA))` to
#' generate empty `<stars>`, and here we specify the function to test the emptiness
#' of such objects.
#'
#' For internal use only.
#'
#' @param x an object of class `<stars>`
#'
#' @return logical, whether x is a `<stars>` object or not
is_stars_empty <- function(x){

  if(!inherits(x, "stars")){
    cli::cli_abort(
      c("Argument {.arg x} must be of class {.cls stars}, not {.cls {class(x)}}"),
      class = "err-arg-wrong-class"
    )
  }

  lapply(x, \(x) all(is.na(x))) |>
    unlist() |>
    all()
}



#' Assertion for empty `<function>` objects
is_empty_function <- function(f){
  check_class(f, "function")
  all.equal(body(f), quote({}))

}




# Helper to define a cli style for a vector. A wrapper of `cli::cli_vec()` to
# simplify calls, allowing the choice of separator and the last word when
# collapsing a vector into a single string. Must be called inside a `cli` definition
vec_style <- function(x, sep = ", ", last = " or "){
  cli::cli_vec(x, style = list("vec-sep" = sep, "vec-last" = last))
}



#' Cast numeric, distributional and unit objects as VarDist
as_vardist <- function(x, units){
  if(inherits(x, "numeric") || distributional::is_distribution(x)){
    x <- VarDist(x, units)
  }else if(is(x, "units")){
    x <- VarDist(units::drop_units(x), units::deparse_unit(x))
  }
  x
}




#' <stars> Slicer
#'
#' A more flexible approach to the dplyr-based `stars.slice()` method, allowing
#' for dynamic slicing over multiple dimensions of a `<stars>` object.
#'
#' This function enables users to extract specific slices along one or more
#' dimensions while optionally dropping singleton dimensions.
#'
#'
#' @param strs a `<stars>` object
#' @param dim_along a integer or character vector specifying the dimensions along
#'   which to slice the `<stars>` array
#' @param ... integer or character vectors providing the indices or values to slice
#'   for each of the dimensions specified in `dim_along`. The order must
#'   match the order of dimensions in `dim_along`.
#' @param .drop logical, drop dimensions that only have a single index after slicing?
#'
#'
#' @return A `<stars>` object containing the sliced subset of the original
#'   multi-dimensional array.
#'
slice_strs <- function(strs, dim_along, ..., .drop = FALSE){

  # TODO: check issue with slicing dimensions of type <Date> and <Posixt>
  # TODO: unit-testing


  # Note: as noted in the package documentation, `<stars>` subsetting using the
  # "[" operator need to take into account that the first argument selects
  # attributes, and dimensions are selected by subsequent arguments. Also,
  # dim.stars() only returns the dimensions. Thus, indexing needs to be done
  # over ndim + 1

  if(is.character(dim_along)){
    if(is.null(dimnames(strs))) cli::cli_abort("`strs` must have named dimensions.")
    dm <- match(dim_along, dimnames(strs))
    missnames <- dim_along[is.na(dm)]
    if(length(missnames) > 0){
      cli::cli_abort("{.val {missnames}} {?is/are} not dimension name{?s} of the provided {.arg strs} object")
    }
  }else if(!is.numeric(dim_along)){
    cli::cli_abort("{.str dim_along} must be a {.cls numeric} vector.")
  }else{
    dm <- dim_along
  }

  # collect subsetting indices for each dimension
  dm_idx <- rlang::list2(...)

  if (length(dm) != length(dm_idx)) {
    cli::cli_abort("Too many indices provided ({length(dm_idx)}) given `length(dim_along) == {length(dm)}`.")
  }

  n_dm <- length(dim(strs))
  indices <- rep(list(rlang::missing_arg()), n_dm + 1)

  # assign specified indices for each dimension
  for(i in seq_along(dm)){
    indices[[dm[i] + 1]] <- dm_idx[[i]]
  }

  # append drop option
  indices[["drop"]] <- .drop

  eval(rlang::expr(strs[!!!indices]))
  #do.call("[", c(list(strs), indices))
}




#' Get element of list of S4 objects
#'
#'  Helper to get element of list of S4 objects using it's slot `@id`.
#'  Applicable to e.g. `IBM@drivers` or `Species@states_profile`
#'
#' @param l a list containing S4 class objects as elements, each of which must contain a slot @id
#' @param id character, the name assigned to @id
#'
#'
#' @examples
#' pluck_s4(rover@states_profile, "water_resting")
#'
#' pluck_s4(rover_ibm_disnbs@drivers, "dens")
#'
#' # returns NULL if there is no element with specified ID
#' pluck_s4(rover_ibm_disnbs@drivers, "water_resting")
#'
pluck_s4 <- function(l, id){

  idx <- which(purrr::map_lgl(l, \(x) x@id == id))

  if(length(idx) != 0){
    purrr::pluck(l, idx)
  } else{
    NULL
  }
}





create_bbox <- function(xmin, ymin, xmax, ymax, crs = NULL){

  stopifnot(xmin < xmax)
  stopifnot(ymin < ymax)

  structure(
    c(xmin, ymin, xmax, ymax),
    names = c("xmin", "ymin", "xmax", "ymax"),
    class = "bbox",
    crs = crs)

}



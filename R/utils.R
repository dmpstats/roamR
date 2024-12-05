# the inverse of %in%
`%notin%` <- Negate(`%in%`)


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
          error = cnd, class = "err_units_string_misspec"
        )
      }}
  )

  # return nothing if check is passed
  invisible()
}


# the imverse of `%in%`
`%notin%` <- Negate(`%in%`)

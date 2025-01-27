# the inverse of %in%
`%notin%` <- Negate(`%in%`)



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




# units checker
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

  if(class(x) != "stars"){
    cli::cli_abort(
      c("Argument {.arg x} must be of class {.cls stars}, not {.cls {class(x)}}"),
      class = "err-arg-wrong-class"
    )
  }

  lapply(x, \(x) all(is.na(x))) |>
    unlist() |>
    all()

}








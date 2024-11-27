#' Validate Object Class
#'
#' A utility function to validate whether an input object belongs to the
#' expected class, providing clear and informative `{cli}`-style error messages.
#'
#' @param x the target object to validate. If a `list`, each element will be
#'   checked against the expected class
#' @param class character string, the name of the expected class of `x`.
#' @param class_fn character string, indicating the function to create objects
#'   of the expected `class`, in the form of "packagename::functionname".
#'
#' @inheritParams rlang::args_error_context
#'
#' @details
#' This is an input checker function, i.e. it is exclusively intended for
#' internal use within other functions to check their inputs - it acts on behalf
#' of other functions as their input validator.
#'
#' If `x` is a `list`, the function checks each element individually, throwing
#' an error if any element does not belong to the expected `class`.
#'
#' `class_fn` is optional, and is used to generate a link in the error message
#' to the function's manual page, if available.
#'
check_class <- function(x,
                        class,
                        class_fn = NULL,
                        arg = rlang::caller_arg(x),
                        call = rlang::caller_env()){

  if(!is(x, class)){
    msg <- c(
      "Argument {.arg {arg}} must be an object of class {.cls {class}}",
      "x" = "You've provided an object of class {.cls {class(x)}}"
    )

    if(!is.null(class_fn)){
      msg[["i"]] <- "Use {.fun {class_fn}} to construct {.cls {class}} objects"
    }

    cli::cli_abort(msg, class = "err-wrong-class", call = call)
  }

}




#' # Input checkers for S4 objects
#'
#'
#' #' Checks to use in S4 validators, which should return a string if an issue is
#' #' found
#' check_slot_length <- function(x, lt = 1){
#'
#'   slnm <- deparse(substitute(x))
#'
#'   if(length(x) != lt){
#'     paste0("Length of slot @", slnm, " must be ", lt)
#'   }else{
#'     invisible()
#'   }
#' }

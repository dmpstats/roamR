#' Validate Object Class
#'
#' A internal input checker function to validate whether an object belongs to the
#' expected class, providing clear and informative `{cli}`-style error messages.
#'
#' @param x the target object to validate. If a `list`, each element will be
#'   checked against the expected class
#' @param class character string, the name of the expected class of `x`.
#' @param inlist logical, whether to take `x` as a list containing objects of the
#'   expected `class`
#' @param class_fn character string, indicating the function to create objects
#'   of the expected `class`, in the form of "packagename::functionname".
#'
#' @inheritParams rlang::args_error_context
#'
#' @details This is an input checker function, i.e. it is intended for internal
#' use within other functions to check their inputs - it acts on behalf of other
#' functions as their input validator.
#'
#' If `x` is a `list`, the function checks each element individually, throwing
#' an error if any element does not belong to the expected `class`.
#'
#' `class_fn` is optional, and is used to generate a link in the error message
#' to the function's manual page, if available.
#'
check_class <- function(x,
                        class,
                        inlist = FALSE,
                        class_fn = NULL,
                        arg = rlang::caller_arg(x),
                        call = rlang::caller_env()){

  msg <- NULL

  if(inlist){

    if(is.list(x)){
      failed_el <- which(!unlist(lapply(x, is, class)))

      if(length(failed_el) > 0){
        msg <- c("Argument {.arg {arg}} must be a {.cls list} comprising objects of class {.cls {class}}")

        for(i in seq_along(failed_el)){
          el_pos <- failed_el[i]
          el_class <- class(x[[el_pos]])
          msg <- c(msg, "x" = paste0("List element #", el_pos, " is an object of class {.cls ", el_class, "}"))
        }
      }

    }else{
      msg <- c(
        "Argument {.arg {arg}} must be a {.cls list} comprising objects of class {.cls {class}}",
        "x" = "You've provided an object of class {.cls {class(x)}}")
    }

  }else{
    if(!is.list(x)){
      if(!is(x, class)){
        msg <- c(
          "Argument {.arg {arg}} must be an object of class {.cls {class}}",
          "x" = "You've provided an object of class {.cls {class(x)}}"
        )
      }
    }else{
      msg <- c("Argument {.arg {arg}} must be an object of class {.cls {class}}",
               "x" = "You've provided an object of class {.cls list}")
    }
  }

  if(!is.null(msg)){
    if(!is.null(class_fn)){
      if(inlist){
        msg <- c(msg, "i" = "Combine {.fun base::list} and {.fun {class_fn}} to construct a {.cls list} of {.cls {class}} objects")
      }else{
        msg <- c(msg, "i" = "Use {.fun {class_fn}} to construct {.cls {class}} objects")
      }
    }

    cli::cli_abort(msg, class = "err-wrong-class", call = call)
  }

  invisible()
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

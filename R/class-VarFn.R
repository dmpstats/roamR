#' Class `<VarFn>`
#'
#' `<VarFn>` is an S4 class that describes the functional properties of a
#' variable of interest. It enables the specification of a variable as a
#' function of one or more predictors through an expression, thereby describing
#' the relationship between the variable and its explanatory factors. This
#' provides a structured approach to incorporating dynamic dependencies between
#' model components. This class extends from the [`VarDist-class`] class,
#' inheriting its slots.
#'
#'
setClass(
  "VarFn",
  contains = "VarDist",
  slots = list(
    fn = "function",
    args_spec = "list",
    fn_cmp = "function"
  ),
  prototype = list(
    fn = function(){},
    args_spec = list(),
    fn_cmp = function(){}
  )
)




#' Create a `<VarFn>` object
#'
#' @param args_spec a list
#' @param fn_cmp compiled `fn`, resulting from applying e.g. `build_cost_fn()`
#'   to `fn`
#'
#' @export
VarFn <- function(fn = NULL,
                  args_spec = NULL,
                  units = NULL,
                  fn_cmp = NULL){

  # input pre-processing -------------------------------------

  ## NULL handling
  fn <- fn %||% function(){}
  fn_cmp <- fn_cmp %||% function(){}
  args_spec <- args_spec %||% list()
  units <- units %||% ""

  ## get names of arguments from `fn`
  fn_arg_nms <- rlang::fn_fmls_names(fn)

  # higher-level input validation -----------------------------------------
  check_class(fn, "function")
  check_class(args_spec, "list")
  check_class(units, "character")
  check_units(units)

  if(length(fn_arg_nms) != length(args_spec)){
    cli::cli_abort(c(
      "Length of {.arg args_spec} must match number of arguments of function passed to {.arg fn}.",
      x = "{.arg fn} has {length(fn_arg_nms)} argument{?s} ({cli::col_blue(fn_arg_nms)}), while `args_spec` has {length(args_spec)} element{?s} ({.field {names(args_spec)}})."
    ))
  }

  # Ensuring naming consistency between `fn` argument and `args_spec` elements ----------------

  ## arg_spec`: set name of elements of class <ArgSpec> to ArgSpec@name
  ## NOTE: it overwrites user-defined names for those elements
  argspec_idx <- sapply(args_spec, \(arg) is(arg, "ArgSpec")) |> unlist()
  names(args_spec)[argspec_idx] <- sapply(args_spec[argspec_idx], \(arg) arg@name)


  if(is.null(names(args_spec))){
    # only issue warning for multiple arguments
    if(length(fn_arg_nms) > 1){
      cli::cli_alert_warning(
        "{.arg args_spec} is unnamed - naming elements based on {.arg fn} definition, by position."
      )
    }
    names(args_spec) <- fn_arg_nms
  } else if(any(names(args_spec) == "")){
    cli::cli_alert_warning(
      "{.arg args_spec} contains unnamed elements. Renaming {.arg args_spec} elements based on {.arg fn} definition, by position."
    )
    missing_names <- which(names(args_spec) == "")
    names(args_spec)[missing_names] <- fn_arg_nms[missing_names]
  }


  missing_args <- setdiff(fn_arg_nms, names(args_spec))
  if(length(missing_args) > 0){
    cli::cli_abort(c(
      "All formal arguments of {.arg fn} must be specified in {.arg args_spec}.",
      x = "{.arg fn} argument{?s} {cli::col_blue(missing_args)} {?is/are} not listed in {.arg args_spec}."
    ))
  }



  # Convert `args_spec` as list of <ArgSpec>s  ----------------------------------
  shortcut_types <- c("driver", "body_mass", "time_at_state")

  args_spec <- purrr::imap(args_spec, function(arg, argname){
    if(is(arg, "ArgSpec")){
      arg
    } else if(is.character(arg) && arg %in% shortcut_types){
      ArgSpec(name = argname, type = arg)
    } else if(is.numeric(arg)){
      ArgSpec(name = argname, type = "constant", value = arg)
    } else if(is(arg, "VarDist")){
      ArgSpec(name = argname, type = "random", distr = distr(arg), units = units(arg))
    } else{
      arg
    }
  })


  invalid_arg_specs <- purrr::discard(args_spec, \(arg) is(arg, "ArgSpec"))

  if(length(invalid_arg_specs) > 0){
    cli::cli_abort(c(
      "Invalid elements listed in {.arg args_spec}.",
      purrr::imap(invalid_arg_specs, \(arg, nm){
        if(!is.character(arg)){
          cli::format_inline("Element {.code ${nm}}: {.code {rlang::expr_text(arg)}} is not a valid class or value.")
        } else{
          cli::format_inline("Element {.code ${nm}}: {.val {arg}} is not a valid class or value.")
        }
      }) |>
        purrr::set_names("x") |>
        unlist(),
      i = "Elements of {.arg args_spec} should be {.cls ArgSpec} or {.cls VarDist} objects. Otherwise, one of {.val {vec_style(shortcut_types)}}."
    ))
  }

  # construct a new instance of <VarFn> ---------------------------
  new(
    "VarFn",
    fn = fn,
    args_spec = args_spec,
    fn_cmp = fn_cmp,
    units = units
  )
}





# Validator -----------------------------------------------------
methods::setValidity("VarFn", function(object) {

  err <- character()

  if(length(object@args_spec) > 0){
    msg <- check_class(object@args_spec, class = "ArgSpec", inlist = TRUE, return_msg = TRUE, arg = "args_spec")
    if(!is.null(msg)) err <- c(err, paste0("\n - ", msg))
  }

  if(length(object@units) > 1){
    err <- c(err, "\n- slot @units must be of length 1")
  }

  if(length(object@distr) > 1){
    err <- c(err, "\n- slot @distr must be of length 1")
  }

  # collect and concatenate error messages
  if(length(err) == 0) TRUE else do.call(paste, list(err, collapse = " "))

})




# Methods -----------------------------------------------------

## Accessors ----

## @args_spec
### getter
setGeneric("args_spec", function(x) standardGeneric("args_spec"))
setMethod("args_spec", "VarFn", function(x) x@args_spec)
### setter
setGeneric("args_spec<-", function(x, value) standardGeneric("args_spec<-"))
setMethod("args_spec<-", "VarFn", function(x, value) {
  x@args_spec <- value
  validObject(x)
  x
})


## @fn_cmp
### getter
setGeneric("fn_cmp", function(x) standardGeneric("fn_cmp"))
setMethod("fn_cmp", "VarFn", function(x) x@fn_cmp)
### setter
setGeneric("fn_cmp<-", function(x, value) standardGeneric("fn_cmp<-"))
setMethod("fn_cmp<-", "VarFn", function(x, value) {
  x@fn_cmp <- value
  validObject(x)
  x
})


## Other ----

#' Empty assertion
#'
#' <VarFn> emptiness evaluation based exclusively on whether `fn` contains empty
#' function
methods::setMethod("is_empty", "VarFn", function(object){
  is_function_empty(object@fn)
})




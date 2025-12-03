#' Introspects `<stars>` objects from the `stars_obj` argument of `Driver()`
#'
#' Internal helper used during the construction of `<Drivers>` objects via
#' [Driver()] to perform validation checks and extract dimensional metadata from
#' the `<stars>` object specified to `stars_obj`. The collected metadata is
#' required for subsequent internal model use, and is intended to be stored in
#' slot `@stars_meta` of `<Driver>` objects.
#'
#' **Important Note**: checks enforced here are specific to the object
#' construction phase within `Driver()`. Additional lower-level validation on
#' `stars_obj` inputs is handled by the dedicated S4 class validator.
#'
#' @param x a `<stars>` object to be validated and classified.
#'
#' @return a list with metadata extracted from `x`

introspect_stars <- function(x,
                             arg = rlang::caller_arg(x),
                             call = rlang::caller_env()){

  # do nothing if stars is empty
  if(is_stars_empty(x)) return(list())

  # Object Validation ------------------------------------------------------------

  ## Only 1 attribute permitted on `stars_obj` at initialization
  if(length(x) > 1){
    cli::cli_abort(c(
      "Argument {.arg {arg}} must be a {.cls stars} object with one single attribute.",
      x = "Provided {.cls stars} object has {length(x)} attributes: {.val {names(x)}}."
    ),
    class = "err-Driver-init-nonsingle-attribute",
    call = call
    )
  }

  ## No more that 4 dimensions
  obj_dims <- dimnames(x)
  if(length(obj_dims) > 4){
    cli::cli_abort(c(
      "{.arg {arg}} cannot have more than 4 dimensions.",
      x = "{.cls stars} object has {length(obj_dims)} dimensions: {.val {obj_dims}}"
    ),
    class = "err-Driver-init-morethan4dims",
    call = call)
  }


  # Dimensions Classification (incl. additional validation)  ---------------------

  ## get names of dimensions
  dimension_names <- dimnames(x)

  # subset names of raster dimensions
  raster_dimension_names <- attr(stars::st_dimensions(x), "raster")$dimensions

  # get list describing raster dimensions
  raster_dimensions <- list(
    dims = match(raster_dimension_names, dimension_names),
    names = raster_dimension_names
  )

  # names of remaining dimensions
  non_raster_dimension_names <- setdiff(dimension_names, raster_dimension_names)

  non_raster_dimensions <- if (length(non_raster_dimension_names) == 0) NULL else {

    lapply(non_raster_dimension_names, function(dimension_name) {

      # get dim values
      dimension_values <- stars::st_get_dimension_values(x, which = dimension_name)

      # dim classification
      if (inherits(dimension_values, c("POSIXt", "Date"))) {

        type <- "temporal"
        proc <- "nearest"

      } else if (is.character(dimension_values)) {

        pattern <- paste0("^(", paste(c(month.name, month.abb), collapse = "|"), ")$")
        is_valid_month_chr <- grepl(pattern, dimension_values, ignore.case = TRUE)

        if(!all(is_valid_month_chr)){
          invalids <- dimension_values[!is_valid_month_chr]
          cli::cli_abort(c(
            "Issue found in dimension {.val {dimension_name}} of {.arg {arg}}.",
            "!" = "Character-valued, non-raster dimensions are assumed to represent months.",
            x = "The following {cli::qty(invalids)} value{?s} in dimension {.val {dimension_name}} are not valid month names: {.val {invalids}}.",
            "i" = "See {.help [month.name](base::month.name)} and {.help [month.abb](base::month.abb)} for accepted values."
          ),
          call = call
          )
        }

        type <- "temporal"
        proc <- "month_chr"

      } else if (is.numeric(dimension_values)) {

        # if numeric, must have the following names
        valid_num_names <- c("month", "year", "quarter", "yearweek", "yearday", "iter", "iteration", "boot", "bootstrap", "sample")

        # standardize dim name, to handle some simple name deviations
        dimension_name_standardized <- tolower(dimension_name) # capitalization
        dimension_name_standardized <- sub("\\.|-", "", dimension_name_standardized) # separating dots/hyphens
        dimension_name_standardized <- sub("s$", "", dimension_name_standardized)   # plurals


        if(dimension_name_standardized %notin% valid_num_names){
          cli::cli_abort(c(
            "Invalid dimension name detected in {.arg {arg}}.",
            "x" = "{.val {dimension_name}} is not an accepted name for a non-raster dimension with numeric values.",
            "i" = "Valid names for such dimensions are: {.val {vec_style(valid_num_names)}}."
          ), call = call)
        }

        # Can't have negative values
        if(any(dimension_values < 0)){
          cli::cli_abort(c(
            "Dimension named {.val {dimension_name}} of {.arg {arg}} contains negative value(s).",
            "x" = "Numeric-valued, non-raster dimensions must contain only positive integers."
          ),
          call = call)
        }

        # Must contain whole/integer numbers
        if(any(!is_whole(dimension_values))){
          cli::cli_abort(c(
            "Dimension named {.val {dimension_name}} of {.arg {arg}} contains fractional value(s).",
            "x" = "Numeric-valued, non-raster dimensions must contain only positive integers."
          ),
          call = call)
        }

        # if years, must have 4 digits
        if(dimension_name_standardized == "year"){
          if(any(n_digits(dimension_values) != 4)){
            cli::cli_abort(c(
              "Issue found in dimension {.val {dimension_name}} of {.arg {arg}}.",
              "x" = "Years must be represented as 4-digit integer numbers (e.g., 1990, 2024)."
            ),
            call = call)
          }}

        # Year time-points must comply with specific boundaries
        switch(
          dimension_name_standardized,
          month = check_bounds_num_dim(dimension_values, dimension_name, "Numeric months", 1, 12, arg, call),
          quarter = check_bounds_num_dim(dimension_values, dimension_name, "Quarters of the year", 1, 4, arg, call),
          yearweek = check_bounds_num_dim(dimension_values, dimension_name, "Week of the year", 1, 52, arg, call),
          yearday = check_bounds_num_dim(dimension_values, dimension_name, "Day of the year", 1, 365, arg, call)
        )


        type <- switch(
          dimension_name_standardized,
          month = "temporal",
          year = "temporal",
          quarter = "temporal",
          yearweek = "temporal",
          yearday = "temporal",
          iter = "iteration",
          iteration = "iteration",
          boot = "iteration",
          bootstrap = "iteration",
          sample = "iteration"
        )

        proc <- switch(
          dimension_name_standardized,
          month = "month_num",
          year = "year",
          quarter = "quarter",
          yearweek = "week",
          yearday = "yday",
          iter = "draw",
          iteration = "draw",
          bootstrap = "draw",
          boot = "draw",
          sample = "draw"
        )
      }

      # non-raster dim 'metadata'
      list(
        dims = which(dimension_name == dimension_names),
        names = dimension_name,
        types = type,
        procs = proc,
        cls = class(dimension_values)[1]
      )
    }) |> purrr::list_transpose()
  }


  # issue warning when dimensions have duplicated types
  if(any(duplicated(non_raster_dimensions$types))){

    dimension_type <- non_raster_dimensions$types[1]
    type_label <- ifelse(non_raster_dimensions$types[1] == "temporal", "time-related variables", "resampling-based replicates")

    cli::cli_warn(c(
      "Non-raster dimensions of {.arg {arg}} must represent distinct covariate types.",
      "!" = "Both dimensions {.val {non_raster_dimensions$names}} appear to convey {type_label}.",
      "!" = "Only the first of these dimensions, {.val {non_raster_dimensions$names[1]}}, will be used in the model."
    ))
  }

  list(raster = raster_dimensions, non_raster = non_raster_dimensions)
}





check_bounds_num_dim <- function(dimension_values, dimension_name, context, min, max, arg, call){
  if(any(dimension_values < min) || any(dimension_values > max)){
    cli::cli_abort(c(
      "Issue found in dimension {.val {dimension_name}} of {.arg {arg}}.",
      "x" = "Dimension contains values outside the accepted range.",
      "x" = "{context} must be represented by integer numbers between {min} and {max} (inclusive)."
    ),
    call = call)
  }
}

# from base::integer() help file
is_whole <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

n_digits <- function(x) nchar( trunc( abs(x) ) )

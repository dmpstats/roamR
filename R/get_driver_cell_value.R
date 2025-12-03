#' Extract cell values from raster-type drivers for the agent's current location
#'
#' @param driver a `<Driver>` object
#' @param agent an `<Agent>` object
#' @param vector_field placeholder
#' For drivers defined by raster-type data, extracts a cell value of the driver
#' given the location of the agent. In addition:
#'    - If a temporal dimension is present, the timestamp to the agent's location
#'    is also used in the extraction
#'    - If a iteration dimension is present, extraction is performed for one
#'    randomly chosen layer.
#'
#' @importFrom rlang !!!
#'
#' @noRd
get_driver_cell_value <- function(driver, agent, vector_field = NULL){

  # TODO
  # - extract value of correct attribute (currently the complement of
  # c(aspect, slope), but maybe make it specific to driver_id instead, once that
  # is a requirement in the <Driver> definition?)

  driver_stars <- stars_obj(driver)

  if(is_stars_empty(driver_stars)){
    cli::cli_abort(c(
      "No raster-type data available for driver {.val {driver_id}}",
      x = "Unable to extract values of {.val {driver_id}} to pass on to the base function"
    ))
  }

  if(length(driver_stars) > 1){
    stars_obj(driver) <- dplyr::select(driver_stars, !dplyr::any_of(c("slope", "aspect")))
  }

  # get agent's current time and location
  agent_location <- sf::st_coordinates(location(agent)) # matrix format
  agent_timestamp <- agent@condition@timestamp

  # get non-raster metadata of driver's <stars> data
  non_raster_metadata <- driver@stars_meta$non_raster

  if(is.null(non_raster_metadata)){ # extract from a raster-only <stars>

    val <- stars::st_extract(driver@stars_obj, at = agent_location)

  } else { # extract from a <stars> with additional non-raster dimensions

    # ensure slicing done for the first dimension of each type
    non_raster_indices <- which(!duplicated(non_raster_metadata$types))

    # non-raster dimensions to slice
    non_raster_dimensions <- non_raster_metadata$dims[non_raster_indices]

    # slice number for each non-raster dimension
    slice_num <- lapply(non_raster_indices, function(idx){
      proc <- non_raster_metadata$procs[idx]
      dim <- non_raster_metadata$dims[idx]
      dimvals <- stars::st_get_dimension_values(driver@stars_obj, dim)

      # convert agent's timestamp to Date if dimension is of type Date
      if(non_raster_metadata$cls[idx] == "Date")  agent_timestamp <- as.Date(agent_timestamp)

      switch (
        proc,
        nearest = nearest_preceding(dimvals, agent_timestamp),
        draw = sample(dimvals, 1),
        month_num = match(lubridate::month(agent_timestamp), dimvals),
        year = match(lubridate::year(agent_timestamp), dimvals),
        quarter = match(lubridate::quarter(agent_timestamp), dimvals),
        week = match(lubridate::week(agent_timestamp), dimvals),
        yday = match(lubridate::yday(agent_timestamp), dimvals),
        month_chr = pmatch(lubridate::month(agent_timestamp, label = TRUE), dimvals)
      )
    })

    # return NA if there is a no match between the agent's data and the driver data
    if(any(is.na(slice_num))){
      val <- NA_real_
    }else{
      val <- driver@stars_obj |>
        slice_strs(non_raster_dimensions, !!!slice_num, .drop = TRUE) |> # bang-bang-bang required for appropriate one-to-many replacement to slice_strs
        stars::st_extract(at = agent_location)
    }
  }

  # HACK - NA handling: if extracted value is NA, return the median of the
  # attribute across all the dimensions
  if(is.na(val)){
    val <- median(driver@stars_obj[[1]], na.rm = TRUE)
  }

  # garbage collection
  rm(driver_stars)

  # subsetting to keep units, if present
  val[[1]]
}





# find index of nearest preceding element of vector - single query
nearest_preceding <- function(x, val){
  # if val is outside the range of x, return NA - i.e. avoid extrapolation
  if(val < min(x) || val > max(x)) return(NA_integer_)
  # among those <= val, pick the one with the largest x[i]
  idxs <- which(x <= val)
  idxs[which.max(x[idxs])]
}




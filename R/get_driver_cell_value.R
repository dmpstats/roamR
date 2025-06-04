#' Extract cell values from raster-type drivers
#'
#' For drivers defined by raster-type data, extracts a cell value of the driver
#' given the location of the agent. In addition:
#'    - If a temporal dimension is present, the timestamp to the agent's location
#'    is also used in the extraction
#'    - If a iteration dimension is present, extraction is performed for one
#'    randomly chosen layer.
#'
#' @importFrom rlang !!!
get_driver_cell_value <- function(driver, agent, vf = NULL){

  # TODO
  # - extract value of correct attribute (currently the complement of
  # c(aspect, slope), but maybe make it specific to driver_id instead, once that
  # is a requirement in the <Driver> definition?)
  # - handle NAs returned from st_extract

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
  loc <- sf::st_coordinates(location(agent)) # matrix format
  tm <- agent@condition@timestamp

  # get non-raster metadata of driver's <stars> data
  nnrst_meta <- driver@stars_meta$non_raster

  if(is.null(nnrst_meta)){ # extract from a raster-only <stars>

    val <- stars::st_extract(driver@stars_obj, at = loc)

  } else { # extract from a <stars> with additional non-raster dimensions

    # ensure slicing done for the first dimension of each type
    nnrst_idxs <- which(!duplicated(nnrst_meta$types))

    # non-raster dimensions to slice
    nnrst_dims <- nnrst_meta$dims[nnrst_idxs]

    # slice number for each non-raster dimension
    slice_num <- lapply(nnrst_idxs, function(idx){
      proc <- nnrst_meta$procs[idx]
      dim <- nnrst_meta$dims[idx]
      dimvals <- stars::st_get_dimension_values(driver@stars_obj, dim)

      switch (
        proc,
        nearest = nearest_preceding(dimvals, tm),
        draw = sample(dimvals, 1),
        month_num = match(lubridate::month(tm), dimvals),
        year = match(lubridate::year(tm), dimvals),
        quarter = match(lubridate::quarter(tm), dimvals),
        week = match(lubridate::week(tm), dimvals),
        yday = match(lubridate::yday(tm), dimvals),
        month_chr = pmatch(lubridate::month(tm, label = TRUE), dimvals)
      )
    })

    # return NA if there is a no match between the agent's data and the driver data
    if(any(is.na(slice_num))){
      val <- NA_real_
    }else{
      val <- driver@stars_obj |>
        slice_strs(nnrst_dims, !!!slice_num, .drop = TRUE) |> # bang-bang-bang required for appropriate one-to-many replacement to slice_strs
        stars::st_extract(at = loc)
    }
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




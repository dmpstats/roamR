#' Initialize the Individual Based Model
#'
#' Sets up the starting conditions and initial state for the IBM simulation,
#' incorporating species, habitat, structures, and configuration settings.
#'
#' @param species an object of class `<Species>`, comprising the species-level
#'   characteristics of the simulated agents (see [Species()])
#' @param drivers ...
#' @param config an object of class `<ModelConfig>`, specifying the primary
#'   configuration settings for the IBM (see [ModelConfig()])
#'
#' @export
#'
rmr_initiate <- function(config, species, drivers){

  ## Input validation ----------------------------------------------------------
  cli::cli_progress_step("Performing input validation")

  if(is(drivers, "Driver")){
    drivers <- list(drivers)
  }else if(!is.list(drivers)){
    cli::cli_abort("{.arg drivers} must be a list of {.cls Driver} objects")
  }

  ### Type checking -----------
  check_class(species, "Species", class_fn = "roamR::Species")
  check_class(drivers, "Driver", inlist = TRUE, class_fn = "roamR::Driver")
  check_class(config, "ModelConfig", class_fn = "roamR::ModelConfig")

  ### Content checking --------
  # TODO Check spatial consistency of drivers given specified AOC (inc CRS)
  # TODO check existence of driver responses
  # TODO check consistency in driver_ids between drivers and driver_responses
  # TODO check uniqueness of driver_ids


  ## AOC Processing -------------------------------------------------------
  cli::cli_progress_step("Processing the AOC")

  #TODO: Add safeguard on handling memory failures due to unreasonable spatial
  #resolution. Maybe use a try_fetch to rephrase the error and provide
  #constructive user feedback
  aoc_grid <- sf::st_make_grid(
    config@aoc_bbx,
    cellsize = c(config@delta_x, config@delta_y),
    what = "centers"
  )

  aoc_grid <- sf::st_sf(cellid = 1:length(aoc_grid), geometry = aoc_grid)

  # generate aoc driver
  aoc_driver <- generate_aoc_driver(config@aoc_bbx, aoc_grid)
  drivers <- append(drivers, aoc_driver)

  # define species response
  aoc_resp <- DriverResponse(
    driver_id = aoc_driver@id,
    movement = MoveInfluence(
      prob = VarDist(distributional::dist_degenerate(1)),  # all agents to be influenced
      fn = \(x) ifelse(x <= 0, 1, 0), # binary influencer with cut-off at bbox's border (i.e. 0m)
      type = "repulsion"
    )
  )

  species@driver_responses <- append(species@driver_responses, aoc_resp)


  ## Driver processing  ------------------------------------------------------
  cli::cli_progress_step("Calculating vector fields for movement drivers")

  ### Compute vector fields for movement influencers ----------------
  drv_ids <- purrr::map_chr(drivers, \(x) x@id)

  # IDs of drivers influencing movement
  drvmv_ids <- species@driver_responses |>
    purrr::keep(\(x) !is_empty(x@movement@prob)) |> # driver doesn't affect movement if @prob in <MoveInfluence> is empty
    purrr::map_chr(\(x) x@driver_id)

  if(length(drvmv_ids) > 0){

    # For each geom-based movement drivers: (i) calculate surface of
    # cell-distances (AOC grid); (ii)  update driver
    drivers <- drivers |>
      purrr::modify_if(
        .p = \(d){ d@id %in% drvmv_ids && is_stars_empty(stars_obj(d))},
        .f = \(d, grid = aoc_grid){
          grid$drv_dist <- sf::st_distance(grid, d@sf_obj)
          stars_obj(d) <- stars::st_rasterize(grid)["drv_dist"]
          d@stars_descr <- paste0("Distance to ", d@sf_descr)
          d@obj_active <- "stars"
          validObject(d)
          d
        })

    # add vector fields for movement-affecting drivers
    drivers <- drivers |>
      purrr::modify_if(
        .p = \(d){ d@id %in% drvmv_ids},
        .f = \(d){
          stars_obj(d) <- compute_vector_fields(stars_obj(d))
          d
        },
        .progress = TRUE
      )

  }else{
     cli::cli_alert_warning("Skipping vector field computation as none of the specified drivers affect movement")
  }




  ## initialize Agents -------------------------------------------------------

  # Agent()
  #

  # IBM(
  #   #agents = list(),
  #   #species = species,
  #   #drivers = drivers,
  #   config = config
  # )


  cli::cli_progress_done()


  cli::cli_alert_danger("This function is under development - No output returned!")



}





#' Generate surface-based spatial driver for distances to bounding box
#'
#' Primarily intended For simulation purposes, so that agents are kept within
#' the area of calculation in the movement model
#'
#' @noRd
generate_aoc_driver <- function(bbox, grid){
  # TODO: (i) documentation; (ii) unit-testing

  # Cast bbox as linestring, for cell-to-boundary distance calculation
  bbox <- sf::st_as_sfc(bbox) |> sf::st_cast("LINESTRING")

  # generate grid distances surface
  grid$bbox_dist <- sf::st_distance(grid, bbox)
  bbox_grid_dist <- stars::st_rasterize(grid)["bbox_dist"]

  Driver(
    id = "aoc",
    type = "model",
    descr = "Distance to AOC's bounding box",
    stars_obj = bbox_grid_dist,
    stars_descr = "Distance to AOC's bounding box",
    obj_active = "stars"
  )

}


#' Compute vector field of a stars raster
#'
#' Calculate the vector fields (rasters aspect and slope) of a stars object for
#' a single attribute, for multi-dimensions
compute_vector_fields <- function(strs){

  if(!inherits(strs, "stars")) cli::cli_abort("{arg. strs} must be a {.cls stars} object")
  if(length(strs) != 1) stop("'strs' must be a single-attribute <stars> object")

  # get the labels of dimensions defining the coords of the spatial grid,
  # i.e. the names used for the x/y dimensions
  xy_labs <- attr(stars::st_dimensions(strs), "raster")$dimensions
  # dimnames for non-grid variables
  cov_labs <- setdiff(dimnames(strs), xy_labs)

  if(length(cov_labs) == 0){
    vfs <- get_slope_aspect(strs)
  }else{
    cov_vals <- sapply(
      cov_labs,
      \(x) stars::st_get_dimension_values(strs, which = x),
      simplify = FALSE
    )

    cov_grid <- expand.grid(cov_vals, stringsAsFactors = FALSE)

    # compute vector fields for each layer
    vfs_ls <- purrr::pmap(cov_grid, \(...){
      cov_val <- list(...)
      slice_strs(strs, cov_labs, !!!cov_val, .drop = TRUE) |>
        get_slope_aspect()
    })

    # combine single layers into original datacube
    vfs <- do.call("c", append(vfs_ls, list(along = cov_vals)))
  }
  vfs
}



# Calculates slope and aspect of one attribute in the stars object and
# binds them to the original stars object as attributes
get_slope_aspect <- function(strs){
  #browser()
  if(!inherits(strs, "stars")) stop("`strs` must be a <stars> object")
  if(length(strs) != 1) stop("`strs` must be a single-attribute <stars> object")
  if(length(dim(strs)) > 2) stop("`strs` cannot have more than 2 dimensions")

  vf <- as(strs, "SpatRaster") |>
    terra::terrain(v = c("aspect", "slope")) |>
    stars::st_as_stars(as_attributes = TRUE)

  # force equal dimensions of original data
  stars::st_dimensions(vf) <- stars::st_dimensions(strs)

  c(strs, vf)
}







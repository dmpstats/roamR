#' Initialize the Individual Based Model
#'
#' Sets up the starting conditions and initial state for the IBM simulation,
#' incorporating species, habitat, structures, and configuration settings.
#'
#' @param species an object of class `<Species>`, comprising the species-level
#'   characteristics of the simulated agents (see [Species()])
#' @param drivers ...
#' @param model_config an object of class `<ModelConfig>`, specifying the primary
#'   configuration settings for the IBM (see [ModelConfig()])
#' @param verbose logical, should feedback on initiation progress be printed in
#'   R console?
#'
#' @export
#'
rmr_initiate <- function(model_config, species, drivers, verbose = TRUE){

  ## Input validation ----------------------------------------------------------
  if(verbose) cli::cli_progress_step("Validating inputs")

  if(is(drivers, "Driver")){
    drivers <- list(drivers)
  }else if(!is.list(drivers)){
    cli::cli_abort("{.arg drivers} must be a list of {.cls Driver} objects")
  }

  ### Type checking -----------
  check_class(species, "Species", class_fn = "roamR::Species")
  check_class(drivers, "Driver", inlist = TRUE, class_fn = "roamR::Driver")
  check_class(model_config, "ModelConfig", class_fn = "roamR::ModelConfig")

  ### Content checking --------
  # TODO Check spatial consistency of drivers given specified AOC (inc CRS)
  # TODO check existence of driver responses
  # TODO check consistency in driver_ids between drivers and driver_responses
  # TODO check uniqueness of driver_ids


  ## AOC Processing -------------------------------------------------------
  if(verbose) cli::cli_progress_step("Processing the AOC")

  # TODO: Add safeguard on handling memory failures due to unreasonable spatial
  #resolution. Maybe use a try_fetch to rephrase the error and provide
  #constructive user feedback
  aoc_grid <- sf::st_make_grid(
    model_config@aoc_bbx,
    cellsize = c(model_config@delta_x, model_config@delta_y),
    what = "centers"
  )

  aoc_grid <- sf::st_sf(cellid = 1:length(aoc_grid), geometry = aoc_grid)

  # generate aoc driver
  aoc_driver <- generate_aoc_driver(model_config@aoc_bbx, aoc_grid)
  drivers <- append(drivers, aoc_driver)

  # define species response
  aoc_resp <- DriverResponse(
    driver_id = aoc_driver@id,
    movement = MoveInfluence(
      prob = VarDist(1),  # all agents to be influenced (p = 1)
      fn = \(x) ifelse(x <= 0, 1, 0), # binary influencer with cut-off at bbox's border (i.e. 0m)
      type = "repulsion"
    )
  )

  species@driver_responses <- append(species@driver_responses, aoc_resp)



  ## Driver processing  ------------------------------------------------------
  if(verbose) cli::cli_progress_step("Calculate vector fields for movement drivers")

  ### Compute vector fields for movement influencers ----------------
  drv_ids <- purrr::map_chr(drivers, \(x) x@id)

  # IDs of drivers influencing movement
  drvmv_ids <- species@driver_responses |>
    purrr::keep(\(x) !is_empty(x@movement@prob)) |> # driver doesn't affect movement if @prob in <MoveInfluence> is empty
    purrr::map_chr(\(x) x@driver_id)

  if(length(drvmv_ids) > 0){

    # For each geom-based movement driver: (i) calculate surface of
    # cell-distances (AOC grid); (ii)  update driver
    drivers <- drivers |>
      purrr::modify_if(
        .p = \(d){ d@id %in% drvmv_ids && is_stars_empty(stars_obj(d))},
        .f = function(d, grid = aoc_grid){
          # forcing unioning to get single vector of grid-point distances when
          # driver contains multiple geoms
          grid$drv_dist <- sf::st_distance(grid, sf::st_union(d@sf_obj))
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
        .f = function(d){
          stars_obj(d) <- compute_vector_fields(stars_obj(d))
          d
        },
        .progress = TRUE
      )

  }else{
     cli::cli_alert_warning("Skipping vector field computation as none of the specified drivers affect movement")
  }


  ## Initialize Agents -------------------------------------------------------
  if(verbose) cli::cli_progress_step("Initialize Agents")

  if (model_config@n_agents > 100 && !is_empty(species)) {
    n_wk <- future::availableCores() - 3
    cli::cli_alert_info("Parallelizing agent initialization across {n_wk} workers")
    future::plan(future::multisession(), workers = n_wk)
  } else {
    future::plan(future::sequential())
  }

  fmt <- "{cli::symbol$info} Initialize Agents {cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"

  agents <- furrr::future_map(
    cli::cli_progress_along(1:model_config@n_agents, current = FALSE, format = fmt),
    function(i){
      Agent(species, model_config)
    },
    .options = furrr::furrr_options(seed = TRUE)
  )



  ## Initialize <IBM> object --------------------------------------------------
  if(verbose) cli::cli_progress_step("Initialize {.cls IBM} object")

  ibm <- IBM(
    agents = agents,
    species = species,
    drivers = drivers,
    model_config = model_config
  )

  if(verbose){
    cli::cli_progress_done()
    #cli::cli_alert_success("All DONE! {emoji::emoji('thumbsup')}")
    cli::cli_text(cli::style_bold("{cli::symbol$star} All DONE!"))
  }

  ibm
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







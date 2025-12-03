#' Initialize the Individual Based Model
#'
#' Sets up the starting conditions and initial state for the IBM simulation,
#' incorporating species, habitat, structures, and configuration settings.
#'
#' @param species an object of class `<Species>`, comprising the species-level
#'   characteristics of the simulated agents (see [Species()])
#' @param drivers ...
#' @param model_config an object of class `<ModelConfig>`, specifying the primary
#'   configuration settings for the IBM (see [ModelConfig()]).
#' @param quiet logical, should feedback on initiation progress be prevented
#'   from being printed in R console?
#'
#' @export
#'
rmr_initiate <- function(model_config, species, drivers, quiet = FALSE){

  ## Input validation ----------------------------------------------------------
  if(!quiet) cli::cli_progress_step("Validating inputs")

  if(is(drivers, "Driver")){
    drivers <- list(drivers)
  }else if(!is.list(drivers)){
    cli::cli_abort("{.arg drivers} must be a list of {.cls Driver} objects")
  }

  check_class(species, "Species", class_fn = "roamR::Species")
  check_class(drivers, "Driver", inlist = TRUE, class_fn = "roamR::Driver")
  check_class(model_config, "ModelConfig", class_fn = "roamR::ModelConfig")


  init_check_consistency(species, drivers, model_config)


  ## AOC Processing -------------------------------------------------------
  if(!quiet) cli::cli_progress_step("Processing the AOC")

  # TODO: Add safeguard on handling memory failures due to unreasonable spatial
  # resolution. Maybe use a try_fetch to rephrase the error and provide
  # constructive user feedback
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
      type = "repulsion",
      mode = "vector-field",
      sim_stage = "bsln-imp"
    )
  )

  species@driver_responses <- append(species@driver_responses, aoc_resp)


  ## Driver processing  ------------------------------------------------------
  if(!quiet) cli::cli_progress_step("Cropping drivers to AOC")

  ### Spatially crop drivers to AOC ---------
  drivers <- drivers |>
    purrr::modify_if(
      function(d){
        if(is_empty(d)) FALSE else if(d@id == "aoc") FALSE else TRUE
      },
      function(d){
        if(d@obj_active == "sf"){
          # assume attributes of each geometry are constant throughout that
          # geometry. This avoids warning in subsequent `sf::st_crop`
          # (https://github.com/r-spatial/sf/issues/406#issuecomment-314152780)
          sf::st_agr(sf_obj(d)) <- "constant"
          sf_obj(d) <- sf::st_crop(sf_obj(d), model_config@aoc_bbx)

        }else if(d@obj_active == "stars"){
          stars_obj(d) <- sf::st_crop(stars_obj(d), model_config@aoc_bbx)
        }
        return(d)
      }
    )


  ### Handle movement-influencing drivers ----------------
  if(!quiet) cli::cli_progress_step("Handling movement-influencing drivers")

  # extract ids of drivers influencing movement
  movement_driver_ids <- species@driver_responses |>
    purrr::keep(\(x) !is_empty(x@movement@prob)) |> # driver doesn't affect movement if @prob in <MoveInfluence> is empty
    purrr::map_chr(\(x) x@driver_id)


  if(length(movement_driver_ids) > 0){

    #### sf-based drivers: derive cell-distance surfaces  ------

    # For each movement-influencing driver without raster-type data:
    # (i) calculate surface of distances from sf object to AOC grid-cells;
    # (ii) update driver's slots accordingly
    drivers <- drivers |>
     purrr::modify_if(
       \(driver) driver@id %in% movement_driver_ids && is_stars_empty(stars_obj(driver)) ,
       function(driver, grid = aoc_grid) {
         # forcing unioning to get single vector of grid-point distances when
         # driver contains multiple geoms
         grid$drv_dist <- sf::st_distance(grid, sf::st_union(driver@sf_obj))
         stars_obj(driver) <- stars::st_rasterize(grid)["drv_dist"]
         driver@stars_descr <- paste0("Distance to ", driver@sf_descr)
         driver@obj_active <- "stars"
         validObject(driver)
         return(driver)
       }
     )

    #### Compute vector fields solely where required ------
    vector_field_driver_ids <- species@driver_responses |>
      purrr::keep(\(x) x@driver_id %in% movement_driver_ids && x@movement@mode == "vector-field") |>
      purrr::map_chr(\(x) x@driver_id)


    if(length(vector_field_driver_ids) > 0){
      if(!quiet) cli::cli_progress_step("Calculate vector fields for drivers {.val {vector_field_driver_ids}}.")

      drivers <- drivers |>
        purrr::modify_if(
          \(driver) driver@id %in% vector_field_driver_ids,
          function(driver) {
            stars_obj(driver) <- compute_vector_fields(stars_obj(driver))
            driver
          },
          .progress = TRUE
        )
    }
  }

  ## Species/States processing  ------------------------------------------------------
  if(!quiet) cli::cli_progress_step("Processing Activity States")

  ### Compile user-defined functions for state's energy costs
  if(length(species@states_profile) > 0){

    species@states_profile <- species@states_profile |>
      purrr::modify_if(
      \(s) is(s@energy_cost, "VarFn"),
      function(s){
        # compile function and store it in appropriate slot `@fn_cmp`
        fn_cmp(s@energy_cost) <- build_cost_fn(s@energy_cost, s@id)
        s
      })
  }


  ## Initialize Agents -------------------------------------------------------
  if(!quiet) cli::cli_progress_step("Initialize Agents")

  if (model_config@n_agents > 100 && !is_empty(species)) {
    num_workers <- future::availableCores() - 3
    cli::cli_alert_info("Parallelizing agent initialization across {num_workers} workers")
    future::plan(future::multisession(), workers = num_workers)
  } else {
    future::plan(future::sequential())
  }

  fmt <- "{cli::symbol$info} Initialize Agents {cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"

  agents <- furrr::future_map(
    cli::cli_progress_along(1:model_config@n_agents, current = FALSE, format = fmt),
    ~Agent(species, model_config),
    .options = furrr::furrr_options(seed = TRUE)
  )

  future::plan(future::sequential())

  ## Initialize <IBM> object --------------------------------------------------
  if(!quiet) cli::cli_progress_step("Initialize {.cls IBM} object")

  ibm <- IBM(
    agents = agents,
    species = species,
    drivers = drivers,
    model_config = model_config
  )

  if(!quiet){
    cli::cli_progress_done()
    cli::cli_alert_success("Initialization Done! {emoji::emoji('rocket')}")
    #cli::cli_text(cli::style_bold("{cli::symbol$star} Initialization DONE!"))
  }

  ibm
}




# ///////////////////////////////////////////////////////////////////////////////
#' Initialization object's consistency check
#'
#' Evaluates consistency amongst components of `roamR`'s IBM initialization
#' function. E.g., asserts that:
#' (i) driver IDs are coherent between specified species configuration and
#' defined drivers;
#' (ii) spatial objects defined in driers are spatially consistent qiith defined
#' AOC
#'
init_check_consistency <- function(species,
                                   drivers,
                                   model_config = NULL,
                                   call = rlang::caller_env()){

  # TODO:
  # - Check if essential slots are populated. E.g. Species@energy_mass_conversion
  #   must be provided

  ## fetch driver IDs
  driver_ids <- sapply(drivers, \(x) x@id)

  ## check driver_id uniqueness
  duplicate_driver_ids <- unique(driver_ids[duplicated(driver_ids)])

  if(length(duplicate_driver_ids) > 0){

    error_msg_items <- lapply(duplicate_driver_ids, function(x, nm){
      duplicate_positions <- which(driver_ids == x)
      cli::format_inline("Driver ID {.val {x}} found at positions {.field {duplicate_positions}} in {.arg drivers}.")
    }) |> purrr::set_names("x")

    cli::cli_abort(c(
      "Driver IDs provided to {.arg drivers} must be unique.",
      error_msg_items |> unlist(),
      i = "Ensure that each driver ID is assigned to a unique entry in the `drivers` list."
    ),
    call = call, class = "err-multiple-driverid")
  }


  ## Ensure driver-responses specified for existent drivers
  driver_response_ids <- sapply(species@driver_responses, \(d) d@driver_id)
  missing_response_drivers <- driver_response_ids[driver_response_ids %notin% driver_ids]

  if(length(missing_response_drivers) > 0){
    cli::cli_abort(c(
      "Driver responses specified in {.arg species@driver_responses} must refer to valid driver IDs in {.arg drivers}.",
      x = "Driver ID{?s} {.val {missing_response_drivers}} not found in {.cls Driver} objects whithin {.arg drivers}.",
      i = "Check if {.arg @driver_id}s in {.cls DriverResponse} objects listed under {.arg species@driver_responses} match those defined in {.arg drivers}."
    ),
    call = call, class = "err-nonexistent-driverid")
  }


  ## Driver-dependent energy cost functions
  purrr::walk(species@states_profile, function(state){
    if(is(state@energy_cost, "VarFn")){
      lapply(state@energy_cost@args_spec, function(arg){
        #browser()
        if(arg@type == "driver"){

          # 1. check presence of driver_id in provided `drivers`
          if(arg@driver_id %notin% driver_ids){
            cli::cli_abort(c(
              "Inconsistency detected in energy cost function for state {.val {state@id}}.",
              x = "Function's argument {.arg {arg@name}} is dependent on driver ID {.val {arg@driver_id}}.",
              x = "Can't find driver ID'd as {.val {arg@driver_id}} in {.arg drivers}.",
              i = "Valid driver IDs are: {.val {vec_style(driver_ids)}}."
            ),
            call = call, class = "err-nonexistent-driverid")
          }

          driver <- purrr::keep(drivers, \(d) d@id == arg@driver_id) |>
            purrr::pluck(1) |>
            stars_obj() |>
            #dplyr::pull(1) |>
            dplyr::select(!dplyr::any_of(c("slope", "aspect")))


          # 2. check if raster data is available
          if(is_stars_empty(driver)){
            cli::cli_abort(c(
              "Inconsistency detected in energy cost function for state {.val {state@id}}.",
              x = "The function's argument {.arg {arg@name}} is dependent on driver ID {.val {arg@driver_id}}.",
              x = "Driver ID {.val {arg@driver_id}} is present in {.arg drivers}, but no raster-type data was found in associated {.cls Driver} object.",
              i = "Currently, arguments based on drivers in energy cost functions can only be used with raster-type data."
            ),
            call = call, class =  "err-nonexistent-raster-in-driver")
          }

          # 3. check if units specified in both ends are convertible
          if(arg@units != ""){

            driver_units <- as.character(units(driver[[1]]))

            if(!units::ud_are_convertible(driver_units, arg@units)){
              cli::cli_abort(c(
                "Inconsistency detected in energy cost function for state {.val {state@id}}.",
                x = "Units specified for {.arg {arg@name}} are incompatible with the units of driver ID {.val {arg@driver_id}} provided in {.arg drivers}.",
                x = "Convertion from {.val {arg@units}} to {.val {driver_units}} in not possible."
              ),
              call = call, class = "err-noncovertible-units")
            }
          }
        }
      })
    }
  })


  ## TODO: State-dependent energy cost functions - ArgSpec@type = "time-at-state"



  ## Spatial consistency between drivers and AOC (inc CRS)
  driver_not_empty <- sapply(drivers, Negate(is_empty))

  if(any(driver_not_empty)){

    lapply(drivers, function(driver){

      ### CRS: Check if driver and nodel_config have matching reference system
      # isolate driver's active spatial object
      driver_object <- slot(driver, paste0(driver@obj_active, "_obj"))
      driver_crs <- sf::st_crs(driver_object)

      if(driver_crs$proj4string != model_config@ref_sys$proj4string){
        cli::cli_abort(c(
          "Driver {.val {driver@id}} must have the same coordinate reference system (CRS) as specified in {.arg model_config}.",
          x = "CRS of active spatial object for {.val {driver@id}}: {.val {driver_crs$Name}} (EPSG: {.val {driver_crs$epsg}})",
          x = "Expected CRS from {.arg model_config@ref_sys}: {.val {model_config@ref_sys$Name}} (EPSG: {.val {model_config@ref_sys$epsg}})"
        ),
        call = call, class = "err-crs-mismatch")
      }

      ### Check spatial overlap between AOC and drivers
      #if(driver@id == "owf_foot") browser()

      aoc_poly <- sf::st_as_sfc(aoc_bbx(model_config))

      if(driver@obj_active == "sf"){

        num_features <- length(sf::st_geometry(driver_object))
        features_outside_aoc <- which(lengths(sf::st_intersects(driver_object, aoc_poly)) == 0)
        num_features_outside_aoc <- length(features_outside_aoc)
        #prop_features_outside_aoc <- num_features_outside_aoc/num_features

        if(num_features_outside_aoc == num_features){
          cli::cli_abort(c(
            "{cli::qty(num_features_outside_aoc)} {?The/All} geometric feature{?s} specified for driver ID {.val {driver@id}} {cli::qty(num_features_outside_aoc)} {?is/are} located outside the AOC's spatial extent.",
            i = "To include this driver, consider expanding the AOC defined in argument {.arg model_config}."
          ),
          call = call, class = "err-driver-outside-aoc")

        } else if(num_features_outside_aoc > 0){
          cli::cli_warn(c(
            "{num_features_outside_aoc}/{num_features} of geometric features in driver ID {.val {driver@id}} did not intersect with the AOC.",
            #"!" = "Features in row{?s} {features_outside_aoc} of {.cls sf} object will be excluded from the simulation.",
            i = "To resolve this warning, consider expanding the AOC or removing non-intersecting features from {.val {driver@id}}."
          ),
          call = call, class = "wrn-driver-partial-aoc" )
        }


      }else if(driver@obj_active == "stars"){

        driver_bbox_poly <- sf::st_as_sfc(sf::st_bbox(driver_object))

        intersect_area <- sf::st_intersection(driver_bbox_poly, aoc_poly) |>
          sf::st_area()

        proportion_covered <- intersect_area/sf::st_area(aoc_poly)
        units(proportion_covered) <- NULL

        if(length(proportion_covered) == 0){

          cli::cli_abort(c(
            "Driver ID {.val {driver@id}} lies completely outside the spatial extent of the specified AOC.",
            i = "To include this driver, consider expanding the AOC defined in argument {.arg model_config}."
          ),
          call = call, class = "err-driver-outside-aoc")

        } else if(proportion_covered <= 0.25){
          cli::cli_warn(c(
            "The extent of driver ID {.val {driver@id}} covers only {round(proportion_covered, 3)*100}% of the specified AOC area.",
            "!" = "As a result, most values extracted from driver {.val {driver@id}} during simulation may be NAs."
          ),
          call = call, class = "wrn-driver-partial-aoc")
        }
      }

    })
  }

  NULL
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
compute_vector_fields <- function(stars_object, unit = "radians"){

  if(!inherits(stars_object, "stars")) cli::cli_abort("{arg. stars_object} must be a {.cls stars} object")
  if(length(stars_object) != 1) stop("'stars_object' must be a single-attribute <stars> object")

  # get the labels of dimensions defining the coords of the spatial grid,
  # i.e. the names used for the x/y dimensions
  xy_labels <- attr(stars::st_dimensions(stars_object), "raster")$dimensions
  # dimnames for non-grid variables
  covariate_labels <- setdiff(dimnames(stars_object), xy_labels)

  if(length(covariate_labels) == 0){
    vector_fields <- get_slope_aspect(stars_object)
  }else{
    covariate_values <- sapply(
      covariate_labels,
      \(x) stars::st_get_dimension_values(stars_object, which = x),
      simplify = FALSE
    )

    covariate_grid <- expand.grid(covariate_values, stringsAsFactors = FALSE)

    # compute vector fields for each layer
    vector_fields_list <- purrr::pmap(covariate_grid, \(...){
      covariate_value <- list(...)
      slice_strs(stars_object, covariate_labels, !!!covariate_value, .drop = TRUE) |>
        get_slope_aspect()
    })

    # combine single layers into original datacube
    vector_fields <- do.call("c", append(vector_fields_list, list(along = covariate_values)))
  }

  ## assign angle units
  vector_fields$aspect <- units::set_units(vector_fields$aspect, unit, mode = "standard")
  vector_fields$slope <- units::set_units(vector_fields$slope, unit, mode = "standard")

  vector_fields
}



# Calculates slope and aspect of one attribute in the stars object and
# binds them to the original stars object as attributes
get_slope_aspect <- function(stars_object, unit = "radians"){
  #browser()
  if(!inherits(stars_object, "stars")) stop("`stars_object` must be a <stars> object")
  if(length(stars_object) != 1) stop("`stars_object` must be a single-attribute <stars> object")
  if(length(dim(stars_object)) > 2) stop("`stars_object` cannot have more than 2 dimensions")

  vector_field <- as(stars_object, "SpatRaster") |>
    terra::terrain(v = c("aspect", "slope"), unit = unit) |>
    stars::st_as_stars(as_attributes = TRUE)

  # force equal dimensions of original data
  stars::st_dimensions(vector_field) <- stars::st_dimensions(stars_object)

  # convert aspect to bearing (i.e. East is 0)
  vector_field$aspect <- -(vector_field$aspect - 0.5*pi)

  c(stars_object, vector_field)
}







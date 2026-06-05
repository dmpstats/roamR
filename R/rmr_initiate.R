#' Initialize the Individual Based Model
#'
#' Sets up the starting conditions and initial state for the IBM simulation,
#' collating species, habitat, structures, and configuration definitions. Sense checks on input consistency are performed at this stage, and spatial inputs are processed to ensure conformity with the model configuration (e.g. CRS, spatial coverage). The function also processes species' states and driver responses to prepare for the simulation phase.
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
rmr_initiate <- function(model_config, species, drivers, quiet = FALSE) {
  # TODO:
  # (i) handling consistency in CRS and curvilinearity between spatial inputs:
  # currently, function performs required transformations, but I'm unsure if
  # this is the best approach - it opens the door to errors and warnings and
  # potential data losses e.g. when using st_warp() to force conformity. The
  # alternative is to simply impose checks and fails on non-conformity, leaving
  # users to ensure inputs conformity requirements are met.

  ## * Top-level input validation ---------------------------------------------

  ### class checks
  if (is(drivers, "Driver")) {
    drivers <- list(drivers)
  } else if (!is.list(drivers)) {
    cli::cli_abort("{.arg drivers} must be a list of {.cls Driver} objects")
  }

  check_class(species, "Species", class_fn = "roamR::Species")
  check_class(drivers, "Driver", inlist = TRUE, class_fn = "roamR::Driver")
  check_class(model_config, "ModelConfig", class_fn = "roamR::ModelConfig")

  ## Model config Vs. Drivers: ensure spatio-temporal integrity  ------------------
  if (!quiet) {
    cli::cli_progress_step("Ensuring spatio-temporal consistency of inputs")
  }

  ### If required:
  ### (i) re-project drivers to chosen CRS under model config
  ### (ii) Warp sample curvilinear rasters into regular grids to allow for AOC cropping

  reproj_drvs <- list()
  crvln_drvs <- c()

  drivers <- drivers |>
    purrr::modify_if(
      ~ !is_empty(.x),
      function(d) {
        #browser()
        if (d@obj_active == "sf") {
          drv_crs <- sf::st_crs(sf_obj(d))

          # if non-matching CRS, re-project via st_transform
          if (drv_crs$proj4string != model_config@ref_sys$proj4string) {
            reproj_drvs[[d@id]] <<- drv_crs$Name
            sf_obj(d) <- sf::st_transform(sf_obj(d), model_config@ref_sys)
          }
        } else if (d@obj_active == "stars") {
          drv_crs <- sf::st_crs(stars_obj(d))

          # if non-matching CRS, re-project via st_transform
          if (drv_crs$proj4string != model_config@ref_sys$proj4string) {
            reproj_drvs[[d@id]] <<- drv_crs$Name
            # Note: st_tranform() always returns curvilinear grid (no loss)
            stars_obj(d) <- sf::st_transform(
              stars_obj(d),
              crs = model_config@ref_sys
            )
            #stars_obj(d) <- stars::st_warp(stars_obj(d), crs = model_config@ref_sys)
          }

          # if raster has curvilinear grid, warp sample into regular grid, for
          # AOC cropping below
          drv_dms <- stars::st_dimensions(stars_obj(d))
          if (attr(drv_dms, "raster")$curvilinear) {
            crvln_drvs <<- c(crvln_drvs, d@id)

            # derive distance threshold for st_warp steps below, based on min cell
            # length of source raster. Cell deltas inferred from ratio between
            # range and nr of cells in each x/y dims. This avoids NAs in the
            # resultant surface
            xdlt <- diff(range(drv_dms[[1]]$values)) / dim(drv_dms)[[1]]
            ydlt <- diff(range(drv_dms[[2]]$values)) / dim(drv_dms)[[2]]
            thresh <- min(xdlt, ydlt)

            # warp re-sample with thresh set to minimum delta in x/y dims
            stars_obj(d) <- stars::st_warp(
              src = stars_obj(d),
              crs = model_config@ref_sys,
              threshold = thresh
            )
          }
        }

        return(d)
      }
    )

  ### report spatial alterations applied to drivers
  if (!quiet) {
    cli::cli_progress_done()

    dv <- cli::cli_div(theme = list(".alert-info" = list("margin-left" = 3)))

    if (length(reproj_drvs) > 0) {
      labs <- sprintf(
        "{.val %s} ({.field %s})",
        names(reproj_drvs),
        reproj_drvs
      ) |>
        cli::ansi_collapse(last = " and ")

      cli::cli_alert_info(
        paste0(
          "{cli::qty(reproj_drvs)}Driver{?s} ",
          labs,
          " transformed to match CRS specified by {.arg model_config} ({.field {model_config@ref_sys$Name}})."
        )
      )
    }

    if (!is.null(crvln_drvs)) {
      cli::cli_alert_info(
        "Raster{?s} of Driver{?s} {.val {crvln_drvs}} warped from curvilinear to regular grid{?s}"
      )
    }

    cli::cli_end(dv)
  }

  ### Further consistency checks (e.g. spatial coverage)
  init_check_consistency(species, drivers, model_config)

  ## Spatially crop drivers to AOC  ----------------------------------------

  if (!quiet) {
    cli::cli_progress_step("Cropping spatial Drivers to AoC")
  }

  drivers <- drivers |>
    purrr::modify_if(
      function(d) {
        if (is_empty(d)) {
          FALSE
        } else if (d@id == "aoc") {
          FALSE
        } else {
          TRUE
        }
      },
      function(d) {
        if (d@obj_active == "sf") {
          # assume attributes of each geometry are constant throughout that
          # geometry. This avoids warning in subsequent `sf::st_crop`
          # (https://github.com/r-spatial/sf/issues/406#issuecomment-314152780)
          sf::st_agr(sf_obj(d)) <- "constant"
          sf_obj(d) <- sf::st_crop(sf_obj(d), model_config@aoc_bbx)
        } else if (d@obj_active == "stars") {
          # crop stars to AOC
          stars_obj(d) <- sf::st_crop(stars_obj(d), model_config@aoc_bbx)
        }
        return(d)
      }
    )

  ## Handle drivers for CRW Movement Model ----------------------------------

  if (model_config@movement_model == "crw") {
    ### Generate Driver for AOC-based vector-field  ----------
    # A raster of distances from regular grid within AOC to its bounding box

    if (!quiet) {
      cli::cli_progress_step("CRW Movement: generating the AOC-based Driver")
    }

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
        prob = VarDist(1), # all agents to be influenced (p = 1)
        fn = \(x) ifelse(x <= 0, 1, 0), # binary influencer with cut-off at bbox's border (i.e. 0m)
        type = "repulsion",
        mode = "vector-field",
        sim_stage = "bsln-imp"
      )
    )

    species@driver_responses <- append(species@driver_responses, aoc_resp)

    ### Compute vector-fields for movement influencing drivers  ---------

    # IDs of relevant drivers
    mv_drvids <- species@driver_responses |>
      purrr::keep(\(x) !is_empty(x@movement@prob)) |> # driver doesn't affect movement if @prob in <MoveInfluence> is empty
      purrr::map_chr(\(x) x@driver_id)

    if (length(mv_drvids) > 0) {
      ### sf-based drivers: derive cell-distance surfaces  -------------

      # For each movement-influencing driver without raster-type data:
      # (i) calculate surface of distances from sf object to AOC grid-cells;
      # (ii) update driver's slots accordingly
      drivers <- drivers |>
        purrr::modify_if(
          \(d) d@id %in% mv_drvids && is_stars_empty(stars_obj(d)),
          function(d, grid = aoc_grid) {
            # forcing unioning to get single vector of grid-point distances when
            # driver contains multiple geoms
            grid$drv_dist <- sf::st_distance(grid, sf::st_union(d@sf_obj))
            stars_obj(d) <- stars::st_rasterize(grid)["drv_dist"]
            d@stars_descr <- paste0("Distance to ", d@sf_descr)
            d@obj_active <- "stars"
            validObject(d)
            return(d)
          }
        )

      #### Compute vector fields solely where required ------
      vf_drvids <- species@driver_responses |>
        purrr::keep(\(x) {
          x@driver_id %in% mv_drvids && x@movement@mode == "vector-field"
        }) |>
        purrr::map_chr(\(x) x@driver_id)

      if (length(vf_drvids) > 0) {
        if (!quiet) {
          cli::cli_progress_step(
            "CRW Movement: {cli::qty(vf_drvids)}calculate vector-field for driver{?s} {.val {vf_drvids}}"
          )
        }

        drivers <- drivers |>
          purrr::modify_if(
            \(d) d@id %in% vf_drvids,
            function(d) {
              stars_obj(d) <- compute_vector_fields(stars_obj(d))
              d
            },
            .progress = TRUE
          )
      }
    }
  }

  ## Species/States processing  ------------------------------------------------------

  ### Compile user-defined functions for state's energy costs
  if (length(species@states_profile) > 0) {
    if (!quiet) {
      stids <- sapply(species@states_profile, \(s) s@id)
      cli::cli_progress_step("Processing Activity States: {.val {stids}}")
    }

    species@states_profile <- species@states_profile |>
      purrr::modify_if(
        \(s) is(s@energy_cost, "VarFn"),
        function(s) {
          # compile function and store it in appropriate slot `@fn_cmp`
          fn_cmp(s@energy_cost) <- build_cost_fn(s@energy_cost, s@id)
          s
        }
      )
  }

  ## Initialize Agents -------------------------------------------------------
  if (!quiet) {
    cli::cli_progress_step("Initializing {model_config@n_agents} Agent{?s}")
  }

  if (model_config@n_agents > 100 && !is_empty(species)) {
    n_wk <- future::availableCores() - 3
    cli::cli_alert_info(
      "Parallelizing agent initialization across {n_wk} workers"
    )
    future::plan(future::multisession(), workers = n_wk)
  } else {
    future::plan(future::sequential())
  }

  fmt <- "{cli::symbol$info} Initialize Agents {cli::pb_bar} {cli::pb_current}/{cli::pb_total} | ETA: {cli::pb_eta}"

  agents <- furrr::future_map(
    cli::cli_progress_along(
      1:model_config@n_agents,
      current = FALSE,
      format = fmt
    ),
    ~ Agent(species, model_config),
    .options = furrr::furrr_options(seed = TRUE)
  )

  future::plan(future::sequential())

  ## Initialize <IBM> object --------------------------------------------------
  if (!quiet) {
    cli::cli_progress_step("Set up {.cls IBM} object")
  }

  ibm <- IBM(
    agents = agents,
    species = species,
    drivers = drivers,
    model_config = model_config
  )

  if (!quiet) {
    cli::cli_progress_done()
    #cli::cli_alert_success("Initialization Done! {emoji::emoji('rocket')}")
    cli::cli_text("")
    cli::cli_text("Model initialization done! {emoji::emoji('rocket')}")
  }

  ibm
}


#' Generate surface-based spatial driver for distances to bounding box
#'
#' Primarily intended For simulation purposes, so that agents are kept within
#' the area of calculation in the movement model
#'
#' @noRd
generate_aoc_driver <- function(bbox, grid) {
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
compute_vector_fields <- function(strs, unit = "radians") {
  if (!inherits(strs, "stars")) {
    cli::cli_abort("{arg. strs} must be a {.cls stars} object")
  }
  if (length(strs) != 1) {
    stop("'strs' must be a single-attribute <stars> object")
  }

  # get the labels of dimensions defining the coords of the spatial grid,
  # i.e. the names used for the x/y dimensions
  xy_labs <- attr(stars::st_dimensions(strs), "raster")$dimensions
  # dimnames for non-grid variables
  cov_labs <- setdiff(dimnames(strs), xy_labs)

  if (length(cov_labs) == 0) {
    vfs <- get_slope_aspect(strs)
  } else {
    cov_vals <- sapply(
      cov_labs,
      \(x) stars::st_get_dimension_values(strs, which = x),
      simplify = FALSE
    )

    cov_grid <- expand.grid(cov_vals, stringsAsFactors = FALSE)

    # compute vector fields for each layer
    vfs_ls <- purrr::pmap(cov_grid, \(...) {
      cov_val <- list(...)
      slice_strs(strs, cov_labs, !!!cov_val, .drop = TRUE) |>
        get_slope_aspect()
    })

    # combine single layers into original datacube
    vfs <- do.call("c", append(vfs_ls, list(along = cov_vals)))
  }

  ## assign angle units
  vfs$aspect <- units::set_units(vfs$aspect, unit, mode = "standard")
  vfs$slope <- units::set_units(vfs$slope, unit, mode = "standard")

  vfs
}


#' Calculates slope and aspect of one attribute in the stars object and
#' binds them to the original stars object as attributes
#'
#' @param strs A `<stars>` object.
#' @param unit Optional. A single string indicating units; default is `"radians"`.
#'
get_slope_aspect <- function(strs, unit = "radians") {
  #browser()
  if (!inherits(strs, "stars")) {
    stop("`strs` must be a <stars> object")
  }
  if (length(strs) != 1) {
    stop("`strs` must be a single-attribute <stars> object")
  }
  if (length(dim(strs)) > 2) {
    stop("`strs` cannot have more than 2 dimensions")
  }

  vf <- as(strs, "SpatRaster") |>
    terra::terrain(v = c("aspect", "slope"), unit = unit) |>
    stars::st_as_stars(as_attributes = TRUE)

  # force equal dimensions of original data
  stars::st_dimensions(vf) <- stars::st_dimensions(strs)

  # convert aspect to bearing (i.e. East is 0)
  vf$aspect <- -(vf$aspect - 0.5 * pi)

  c(strs, vf)
}

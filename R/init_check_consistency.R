# ///////////////////////////////////////////////////////////////////////////////
#' Initialization object's consistency check
#'
#' Evaluates consistency amongst components of `roamR`'s IBM initialization
#' function. E.g., asserts that:
#' (i) driver IDs are coherent between specified species configuration and
#' defined drivers;
#' (ii) spatial objects defined in drivers are spatially consistent with defined
#' AOC
#'
init_check_consistency <- function(
  species,
  drivers,
  model_config = NULL,
  call = rlang::caller_env()
) {
  # TODO:
  # - Check if essential slots are populated. E.g. Species@energy_mass_conversion
  #   must be provided

  ## fetch driver IDs
  drv_ids <- sapply(drivers, \(x) x@id)

  ## check driver_id uniqueness
  drv_id_dup <- unique(drv_ids[duplicated(drv_ids)])

  if (length(drv_id_dup) > 0) {
    error_msg_items <- lapply(drv_id_dup, function(x, nm) {
      dup_pos <- which(drv_ids == x)
      cli::format_inline(
        "Driver ID {.val {x}} found at positions {.field {dup_pos}} in {.arg drivers}."
      )
    }) |>
      purrr::set_names("x")

    cli::cli_abort(
      c(
        "Driver IDs provided to {.arg drivers} must be unique.",
        error_msg_items |> unlist(),
        i = "Ensure that each driver ID is assigned to a unique entry in the `drivers` list."
      ),
      call = call,
      class = "err-multiple-driverid"
    )
  }

  ## Ensure driver-responses specified for existent drivers ----
  drv_resp_ids <- sapply(species@driver_responses, \(d) d@driver_id)
  missing_resp_drvs <- drv_resp_ids[drv_resp_ids %notin% drv_ids]

  if (length(missing_resp_drvs) > 0) {
    cli::cli_abort(
      c(
        "Driver responses specified in {.arg species@driver_responses} must refer to valid driver IDs in {.arg drivers}.",
        x = "Driver ID{?s} {.val {missing_resp_drvs}} not found in {.cls Driver} objects whithin {.arg drivers}.",
        i = "Check if {.arg @driver_id}s in {.cls DriverResponse} objects listed under {.arg species@driver_responses} match those defined in {.arg drivers}."
      ),
      call = call,
      class = "err-nonexistent-driverid"
    )
  }

  # States checks -----------------------------------------------------------

  ## Activity types  -----------

  ## For density-informed movement models, check that required activity types are represented in states profile, and that they are not duplicated across states
  if (model_config@movement_model == "di") {
    # activity types in states
    actvs <- purrr::map_chr(species@states_profile, ~ .x@type)
    # required activity types for density-informed movement models
    req_actvs <- c("foraging", "resting")
    missing_actvs <- setdiff(req_actvs, actvs)

    if (length(missing_actvs) > 0) {
      cli::cli_abort(
        c(
          "Required activity types missing from {.arg species@states_profile}.",
          x = "Activity type{?s} {.val {missing_actvs}} {?is/are} not assigned to any state.",
          i = "Density-informed movement requires activity types {.val {req_actvs}} to be represented in the states profile."
        ),
        call = call,
        class = "err-missing-activity-types"
      )
    }

    actv_tally <- table(actvs)
    dupl_actvs <- names(actv_tally)[actv_tally > 1]
    req_dupl_actvs <- dupl_actvs[dupl_actvs %in% req_actvs]

    if (length(req_dupl_actvs) > 0) {
      state_ids <- purrr::map_chr(species@states_profile, ~ .x@id)

      dupl_actv_state_ids <- purrr::map_chr(
        req_dupl_actvs,
        function(actv) {
          cli::format_inline(
            "{cli::symbol$bullet} {.val {actv}} is in state IDs {.val {state_ids[which(actvs == actv)]}}"
          )
        }
      )

      names(dupl_actv_state_ids) <- rep(" ", length(dupl_actv_state_ids))

      cli::cli_abort(
        c(
          "Duplicate activity types detected in {.arg species@states_profile}.",
          x = "{cli::qty(req_dupl_actvs)}Activity type{?s} assigned to multiple states:",
          dupl_actv_state_ids,
          i = "Density-informed movement requires activity types {.val {req_actvs}} to be assigned to exactly one state."
        ),
        call = call,
        class = "err-duplicate-activity-types"
      )
    }
  }

  ## Driver-dependent energy cost functions ------
  purrr::walk(species@states_profile, function(st) {
    if (is(st@energy_cost, "VarFn")) {
      lapply(st@energy_cost@args_spec, function(arg) {
        #browser()
        if (arg@type == "driver") {
          # 1. check presence of driver_id in provided `drivers`
          if (arg@driver_id %notin% drv_ids) {
            cli::cli_abort(
              c(
                "Inconsistency detected in energy cost function for state {.val {st@id}}.",
                x = "Function's argument {.arg {arg@name}} is dependent on driver ID {.val {arg@driver_id}}.",
                x = "Can't find driver ID'd as {.val {arg@driver_id}} in {.arg drivers}.",
                i = "Valid driver IDs are: {.val {vec_style(drv_ids)}}."
              ),
              call = call,
              class = "err-nonexistent-driverid"
            )
          }

          drv <- purrr::keep(drivers, \(d) d@id == arg@driver_id) |>
            purrr::pluck(1) |>
            stars_obj() |>
            #dplyr::pull(1) |>
            dplyr::select(!dplyr::any_of(c("slope", "aspect")))

          # 2. check if raster data is available
          if (is_stars_empty(drv)) {
            cli::cli_abort(
              c(
                "Inconsistency detected in energy cost function for state {.val {st@id}}.",
                x = "The function's argument {.arg {arg@name}} is dependent on driver ID {.val {arg@driver_id}}.",
                x = "Driver ID {.val {arg@driver_id}} is present in {.arg drivers}, but no raster-type data was found in associated {.cls Driver} object.",
                i = "Currently, arguments based on drivers in energy cost functions can only be used with raster-type data."
              ),
              call = call,
              class = "err-nonexistent-raster-in-driver"
            )
          }

          # 3. check if units specified in both ends are convertible
          if (arg@units != "") {
            drv_units <- as.character(units(drv[[1]]))

            if (!units::ud_are_convertible(drv_units, arg@units)) {
              cli::cli_abort(
                c(
                  "Inconsistency detected in energy cost function for state {.val {st@id}}.",
                  x = "Units specified for {.arg {arg@name}} are incompatible with the units of driver ID {.val {arg@driver_id}} provided in {.arg drivers}.",
                  x = "Convertion from {.val {arg@units}} to {.val {drv_units}} in not possible."
                ),
                call = call,
                class = "err-noncovertible-units"
              )
            }
          }
        }
      })
    }
  })

  ## TODO: State-dependent energy cost functions - ArgSpec@type = "time-at-state"

  ## Spatial consistency between drivers and model configuration -------
  driver_not_empty <- sapply(drivers, Negate(is_empty))

  if (any(driver_not_empty)) {
    lapply(drivers, function(d) {
      drv_obj <- slot(d, paste0(d@obj_active, "_obj"))

      ### CRS: Check if driver and ModelConfig have matching reference system
      drv_crs <- sf::st_crs(drv_obj)

      if (drv_crs$proj4string != model_config@ref_sys$proj4string) {
        cli::cli_abort(
          c(
            "Driver {.val {d@id}} must have the same coordinate reference system (CRS) as specified in {.arg model_config}.",
            x = "CRS of active spatial object for {.val {d@id}}: {.val {drv_crs$Name}} (EPSG: {.val {drv_crs$epsg}})",
            x = "Expected CRS from {.arg model_config@ref_sys}: {.val {model_config@ref_sys$Name}} (EPSG: {.val {model_config@ref_sys$epsg}})"
          ),
          call = call,
          class = "err-crs-mismatch"
        )
      }

      ### Check spatial overlap between AOC and drivers
      aoc_poly <- sf::st_as_sfc(aoc_bbx(model_config))

      if (d@obj_active == "sf") {
        n_feats <- length(sf::st_geometry(drv_obj))
        feats_out_aoc <- which(
          lengths(sf::st_intersects(drv_obj, aoc_poly)) == 0
        )
        n_feats_out_aoc <- length(feats_out_aoc)
        #prop_feats_out_aoc <- n_feats_out_aoc/n_feats

        if (n_feats_out_aoc == n_feats) {
          cli::cli_abort(
            c(
              "{cli::qty(n_feats_out_aoc)} {?The/All} geometric feature{?s} specified for driver ID {.val {d@id}} {cli::qty(n_feats_out_aoc)} {?is/are} located outside the AOC's spatial extent.",
              i = "To include this driver, consider expanding the AOC defined in argument {.arg model_config}."
            ),
            call = call,
            class = "err-driver-outside-aoc"
          )
        } else if (n_feats_out_aoc > 0) {
          cli::cli_warn(
            c(
              "{n_feats_out_aoc}/{n_feats} of geometric features in driver ID {.val {d@id}} did not intersect with the AOC.",
              #"!" = "Features in row{?s} {feats_out_aoc} of {.cls sf} object will be excluded from the simulation.",
              i = "To resolve this warning, consider expanding the AOC or removing non-intersecting features from {.val {d@id}}."
            ),
            call = call,
            class = "wrn-driver-partial-aoc"
          )
        }
      } else if (d@obj_active == "stars") {
        drv_bbox_poly <- sf::st_as_sfc(sf::st_bbox(drv_obj))

        intersect_area <- sf::st_intersection(drv_bbox_poly, aoc_poly) |>
          sf::st_area()

        prop_covered <- intersect_area / sf::st_area(aoc_poly)
        units(prop_covered) <- NULL

        if (length(prop_covered) == 0) {
          cli::cli_abort(
            c(
              "Driver ID {.val {d@id}} lies completely outside the spatial extent of the specified AOC.",
              i = "To include this driver, consider expanding the AOC defined in argument {.arg model_config}."
            ),
            call = call,
            class = "err-driver-outside-aoc"
          )
        } else if (prop_covered <= 0.25) {
          cli::cli_warn(
            c(
              "The extent of driver ID {.val {d@id}} covers only {round(prop_covered, 3)*100}% of the specified AOC area.",
              "!" = "As a result, most values extracted from driver {.val {d@id}} during simulation may be NAs."
            ),
            call = call,
            class = "wrn-driver-partial-aoc"
          )
        }
      }

      ### Check if start/end points fully contained in density maps, for density-informed movement model
      if (d@obj_active == "stars") {
        # extract species response to driver
        drv_sp_resp <- purrr::detect(species@driver_responses, \(x) {
          x@driver_id == d@id
        })

        if (not_null(drv_sp_resp)) {
          if (
            drv_sp_resp@movement@mode == "cell-value" &&
              drv_sp_resp@movement@sim_stage %in% c("bsln", "bsln-imp") &&
              model_config@movement_model == "di"
          ) {
            # Unionise driver raster extent to a single polygon for coverage checks
            drv_poly <- d@stars_obj |>
              sf::st_as_sf(as_points = FALSE) |>
              sf::st_union(by_feature = FALSE)

            for (slot_nm in c("start_sites", "end_sites")) {
              # slot_nm <- "start_sites"

              sites <- slot(model_config, slot_nm)

              # no sites defined
              if (nrow(sites) == 0) {
                return(invisible(NULL))
              }

              # enquire if sites are within driver's polygon
              #in_drv <- sf::st_contains(drv_poly, sites, sparse = FALSE)
              in_drv <- sf::st_covered_by(sites, drv_poly, sparse = FALSE)
              # identify sites not fully covered
              off_sites <- sites$id[!in_drv]
              # throw error, identifying offending sites
              n_off_sites <- length(off_sites)
              if (n_off_sites > 0) {
                label <- sub("_sites", "", slot_nm)
                label_cap <- paste0(
                  toupper(substr(label, 1, 1)),
                  substr(label, 2, nchar(label))
                )
                cli::cli_abort(
                  c(
                    "{label_cap} {cli::qty(n_off_sites)} site{?s} {.val {off_sites}} not contained within movement-informing driver {.val {d@id}}.",
                    x = "Under movement model {.val di}, all {.arg model_config@{slot_nm}} must be bounded by reference drivers."
                  ),
                  call = call,
                  class = "err-start-end-sites-offside-driver"
                )
              }
            }
          }
        }
      }
    })
  }

  NULL
}

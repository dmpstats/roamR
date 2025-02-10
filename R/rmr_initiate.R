#' Initialize the Individual Based Model
#'
#' Sets up the starting conditions and initial state for the IBM simulation,
#' incorporating species, habitat, structures, and configuration settings.
#'
#' @param species an object of class `<Species>`, comprising the species-level
#'   characteristics of the simulated agents (see [Species()])
#' @param habitat an object of class `<Habitat>`, detailing habitat-level
#'   features of the Area of Calculation (AOC) where the simulation takes place
#'   (see [Habitat()])
#' @param structures a `list` consisting exclusively of objects of class
#'   `<Structure>` [see Structure()], listing man-made physical structures within the
#'   AOC that may influence the agents' movement
#' @param config an object of class `<ModelConfig>`, specifying the primary
#'   configuration settings for the IBM (see [ModelConfig()])
#'
#' @export
#'
rmr_initiate <- function(species, habitat, structures, config){

  ## input validation
  check_class(species, "Species", class_fn = "roamR::Species")
  check_class(habitat, "Habitat", class_fn = "roamR::Habitat")
  check_class(structures, "Structure", inlist = TRUE, class_fn = "roamR::Struture")
  check_class(config, "ModelConfig", class_fn = "roamR::ModelConfig")


  ## initialize Species



  ## initialize Agents

  IBM(
    agents = list(),
    species = species,
    habitat = habitat,
    structures = list(),
    config = config
  )

}
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







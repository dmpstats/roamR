#' Shortest path calc
#'
#' @param density_map A raster density map to sample from
#' @param imp_dens A raster density map under impact to sample from
#' @param agent A roamR agent - mainly for its current location
#'
#' @returns A path
#' @export
#'
#' @examples TBD
path_calc <- function(density_map, imp_dens, agent) {

  zero_path <- T

  while(zero_path == T) {

  finish <- roamR::sample_cell(density_map, 1) |>
    sf::st_point() |>
    sf::st_sfc(crs = "epsg:32630")

  imp_end_cell_val <- stars::st_extract(imp_dens, finish)[[1]]

  # if cell value is NA relocate endpoint to the closest populated cell in the impacted density surface
  if(is.na(imp_end_cell_val)){
    imp_dens_sf <- sf::st_as_sf(imp_dens, as_points = TRUE)
    new_end_imp_dens_idx <- sf::st_nearest_feature(finish, imp_dens_sf)
    finish <- imp_dens_sf$geometry[new_end_imp_dens_idx]
    rm(imp_dens_sf) # garbage collection
  }

  start <- sf::st_sfc(agent@condition@location, crs =  "epsg:32630")

  path_line <- spaths::shortest_paths(terra::rast(density_map), origins = finish, destinations = start, output = "lines") %>%
    sf::st_as_sf()


  if(units::drop_units(sf::st_length(path_line)) > 1000) { zero_path <- F }

  }

  move_pts <- sf::st_line_sample(path_line$geometry, n = 32, type = "regular")

  move_pts

}

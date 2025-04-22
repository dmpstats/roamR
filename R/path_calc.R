#' Shortest path calc
#'
#' @param density_map A raster density map to sample from
#' @param agent A roamR agent - mainly for its current location
#'
#' @returns A path
#' @export
#'
#' @examples TBD
path_calc <- function(density_map, agent) {

  finish <- roamR::sample_cell(density_map, 1) |>
    sf::st_point() |>
    sf::st_sfc(crs = "epsg:32630")

  start <- sf::st_sfc(agent@condition@location, crs =  "epsg:32630")

  path_line <- spaths::shortest_paths(terra::rast(density_map), origins = finish, destinations = start, output = "lines") %>%
    sf::st_as_sf()

  move_pts <- sf::st_line_sample(path_line$geometry, n = 32, type = "regular") %>%
    sf::st_jitter(amount = 500)

  move_pts

}

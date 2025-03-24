#' Sampling a density map as PDF
#'
#' @param in_rast A (stars) raster object
#' @param n_samp How many samples to draw
#'
#' @returns A matrix of x,y coordinates, which are the centre of the sampled raster cells
#' @export
#'
#' @examples sample_cell(density_raster, 10)
sample_cell <- function(in_rast, n_samp){

  n_row <- nrow(in_rast[[1]])

  rast_vect <- in_rast[[1]] %>%
    as.vector() %>%
    if_else(is.na(.), 0, .)

  p_vect <- rast_vect/sum(rast_vect)

  samp_ind <- sample(1:length(p_vect), n_samp, prob = p_vect, replace = T)

  terra::xyFromCell(terra::rast(in_rast), samp_ind)


}

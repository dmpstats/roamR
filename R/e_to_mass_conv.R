#' Title Energy to mass conversion
#'
#' @param x A vector of values (the time steps)
#' @param e_vect A vector of energy values
#' @param bw A bandwidth for smoothing
#'
#' @returns A vector of wts
#' @export
#'
#' @examples TBD
e_to_mass_conv <- function(x, e_vect, bw, conv_c){

  stats::ksmooth(x = x, y = e_vect*conv_c, kernel = "normal", bandwidth = bw)$y

}

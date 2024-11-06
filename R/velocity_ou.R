#' Find 2D velocity vector of an Ornstein-Uhlenbeck process
#'
#' @param driftV A 2d numeric vector representing the drift velocity (nominally m/s) at current location
#' @param beta The beta parameter controlling the correlation of velocities over 1-delta timesteps
#' @param currentV A 2D numeric vector representing the current velocity (nominally m/s)
#' @param delta Numeric, giving timesteps i.e. temporal resolution in seconds e.g. 60 means 1-minute step
#' @param sigma Single numeric, the standard deviation of the velocity noise at 1 delta time resolution
#'
#' @return A 2D numeric (velocity) vector
#' @export

#' @examples
#' # drift to the NE of approx 1m/s, high correlation,
#' # current velocity SW of approx 1m/s, 10s step, 2.5m/s std dev
#' velocity_ou(c(1, 1), 0.1, c(-1, -1), 10, 2.5)
velocity_ou <- function(driftV, beta, currentV, delta, sigma){

  driftV*delta + exp(-beta*delta)*(currentV - driftV) + rnorm(2, 0, sigma^2*(1-exp(-2*beta*delta))/(2*beta))

}

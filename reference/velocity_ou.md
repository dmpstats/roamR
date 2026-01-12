# Find 2D velocity vector of an Ornstein-Uhlenbeck process

Find 2D velocity vector of an Ornstein-Uhlenbeck process

## Usage

``` r
velocity_ou(driftV, beta, currentV, delta, sigma)
```

## Arguments

- driftV:

  A 2d numeric vector representing the drift velocity (nominally m/s) at
  current location

- beta:

  The beta parameter controlling the correlation of velocities over
  1-delta timesteps

- currentV:

  A 2D numeric vector representing the current velocity (nominally m/s)

- delta:

  Numeric, giving timesteps i.e. temporal resolution in seconds e.g. 60
  means 1-minute step

- sigma:

  Single numeric, the standard deviation of the velocity noise at 1
  delta time resolution

## Value

A 2D numeric (velocity) vector

## Examples

``` r
# drift to the NE of approx 1m/s, high correlation,
# current velocity SW of approx 1m/s, 10s step, 2.5m/s std dev
velocity_ou(c(1, 1), 0.1, c(-1, -1), 10, 2.5)
#> [1]  15.27005 -16.73425
```

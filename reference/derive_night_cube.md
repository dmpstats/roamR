# Derive rasters of night-time proportion per day

Helper to compute a datacube comprising spatio-temporal maps of
night-time proportions, within simulated AOC and over the simulated time
period

## Usage

``` r
derive_night_cube(aoc_strs, start_date, end_date, delta_time = "1 week")
```

## Details

NOTES:

- uses
  [`geosphere::daylength()`](https://rdrr.io/pkg/geosphere/man/daylength.html)
  which requires latitude values. Thus, input `aoc_strs` object needs to
  be projected/resampled into "EPSG:4326" for computations, before being
  projected back to its original CRS

- Temporal dimension of returned is always Date

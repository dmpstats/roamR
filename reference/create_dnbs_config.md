# Helper to extract data and info required for configuring DisNBS's agent simulation model

Quick notes, some of them worthy of a reference in details of parent
function
[`run_disnbs()`](https://dmpstats.github.io/roamR/reference/run_disnbs.md):

- If density driver has no temporal dimension, then the density raster
  is the same across all model time-steps

- currently looks for exact correspondence between time-grid and the
  values of temporal dimension of the density driver. This means any
  miss-match between the models expected time-steps and the available
  data will result in an error. Might make sense to relax this approach
  in future dev iterations, e.g. to take the nearest preceding density
  slice

## Usage

``` r
create_dnbs_config(
  dens_drv,
  ibm_cfg,
  ids,
  waypnts_res = units::set_units(100, "m"),
  bmsm_opts = bm_smooth_opts(),
  call = rlang::caller_env()
)
```

## Value

an object of class \<disnbs_config\> with listed parameters required for
input argument `config` of
[`simulate_agent_disnbs()`](https://dmpstats.github.io/roamR/reference/simulate_agent_disnbs.md)

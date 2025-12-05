# Initialize the Individual Based Model

Sets up the starting conditions and initial state for the IBM
simulation, incorporating species, habitat, structures, and
configuration settings.

## Usage

``` r
rmr_initiate(model_config, species, drivers, quiet = FALSE)
```

## Arguments

- model_config:

  an object of class `<ModelConfig>`, specifying the primary
  configuration settings for the IBM (see
  [`ModelConfig()`](https://dmpstats.github.io/roamR/reference/ModelConfig.md)).

- species:

  an object of class `<Species>`, comprising the species-level
  characteristics of the simulated agents (see
  [`Species()`](https://dmpstats.github.io/roamR/reference/Species.md))

- drivers:

  ...

- quiet:

  logical, should feedback on initiation progress be prevented from
  being printed in R console?

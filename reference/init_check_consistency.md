# Initialization object's consistency check

Evaluates consistency amongst components of `roamR`'s IBM initialization
function. E.g., asserts that: (i) driver IDs are coherent between
specified species configuration and defined drivers; (ii) spatial
objects defined in driers are spatially consistent qiith defined AOC

## Usage

``` r
init_check_consistency(
  species,
  drivers,
  model_config = NULL,
  call = rlang::caller_env()
)
```

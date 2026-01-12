# Utility functions for `<VarDist>` objects

- `distr()` returns the
  [`<distribution>`](https://pkg.mitchelloharawild.com/distributional/reference/distributional-package.html)
  object stored in the `distr` slot.

&nbsp;

- `units()` retrieves the value of the `units` slot.

&nbsp;

- [`generate()`](https://generics.r-lib.org/reference/generate.html)
  randomly samples values from the distributional properties of the
  variable.

&nbsp;

- [`parameters()`](https://pkg.mitchelloharawild.com/distributional/reference/parameters.html)
  returns the parameters that define variable's distribution.

## Usage

``` r
# S4 method for class 'VarDist'
distr(x)

# S4 method for class 'VarDist'
units(x)

# S4 method for class 'VarDist'
generate(x, times = 1)

# S4 method for class 'VarDist'
parameters(x)
```

## Arguments

- x:

  an object of class `<VarDist>`.

- times:

  the sample size.

## Examples

``` r
library(distributional)

# create a VarDist object
speed <- VarDist(dist_normal(10, 0.5), "m/s")

# get slot `units`
units(speed)
#> [1] "m/s"

# get slot `distr`
distr(speed)
#> <distribution[1]>
#> [1] N(10, 0.25)

# generate 100 random values from the variable's probability distribution
generate(speed, times = 100)
#> Error in UseMethod("generate"): no applicable method for 'generate' applied to an object of class "VarDist"

# parameters underpinning Normally distributed `speed`
parameters(speed)
#> Error in UseMethod("parameters"): no applicable method for 'parameters' applied to an object of class "VarDist"

```

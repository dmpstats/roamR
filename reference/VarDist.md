# Create a `<VarDist>` object

Helper function to construct an instance of a
\<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>
object, which defines a variable of interest in terms of its probability
distribution, sampling distribution, percentile distribution or fixed
point estimate.

## Usage

``` r
VarDist(distr = NULL, units = NULL)
```

## Arguments

- distr:

  either an object of class
  [`<distribution>`](https://pkg.mitchelloharawild.com/distributional/reference/distributional-package.html)
  or a numeric value. Specifies the distribution of values of the
  variable, representing its expected value and uncertainty/variability.
  If a numeric value is provided, the variable is assumed to be constant
  and will remain fixed throughout the simulation.

- units:

  a character string, defining the measurement units of the variable.
  Must be either a name (e.g. `"grams"`) or a symbol (e.g. `"m/s"`) that
  recognized by the "udunits" database (see
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)).
  If `NULL` (default) the variable is assumed to be unitless.

## Value

an object of class `<VarDist>`

## Details

`<VarDist>` objects extend the functionality of the commendable
[distributional](https://pkg.mitchelloharawild.com/distributional/reference/distributional-package.html)
package by integrating measurement units, ensuring that variable values
are interpreted and processed correctly during simulation calculations.

## See also

Package
[distributional](https://pkg.mitchelloharawild.com/distributional/reference/distributional-package.html)
for access to and details on a comprehensive selection of distributions.

## Examples

``` r
library(distributional)

# define a Normally distributed variable with units m/s
VarDist(dist_normal(mean = 23, sd = 2), "m/s")
#> An object of class "VarDist"
#> Slot "distr":
#> <distribution[1]>
#> [1] N(23, 4)
#> 
#> Slot "units":
#> [1] "m/s"
#> 


# define a parameter with fixed value
VarDist(10, "m")
#> An object of class "VarDist"
#> Slot "distr":
#> <distribution[1]>
#> [1] 10
#> 
#> Slot "units":
#> [1] "m"
#> 

# set variable's empirical distribution from a random sample (e.g a bootstrap)
boot <- rlnorm(100, 2, 1)
mass <- VarDist(dist_sample(list(boot)), "kg")
# re-sample 100 values
generate(mass, times = 100)
#> Error in UseMethod("generate"): no applicable method for 'generate' applied to an object of class "VarDist"
```

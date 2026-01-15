# Class `<VarDist>`

`<VarDist>` is an S4 class that encapsulates the distributional
properties of a variable of interest. It allows variables to be defined
in terms of their probability distribution, sampling distribution,
percentile distribution or a fixed value. The class provides a
structured approach to specifying input values by allowing the
quantification of uncertainty while ensuring proper handling of
measurement units.

## Details

`<VarDist>` objects extend the functionality of the commendable
[distributional](https://pkg.mitchelloharawild.com/distributional/reference/distributional-package.html)
package by incorporating measurement units, ensuring that variable
values are interpreted and processed correctly during simulation
calculations.

## Slots

- `distr`:

  an object of class
  [`<distribution>`](https://pkg.mitchelloharawild.com/distributional/reference/distributional-package.html).
  Specifies the distribution of values of the variable, representing its
  expected value and uncertainty/variability.

- `units`:

  a character string, defining the measurement units of the variable.
  Must be either a name (e.g. `"grams"`) or a symbol (e.g. `"m/s"`) that
  recognized by the "udunits" database (see
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)).
  If `NULL` (default) the variable is assumed to be unitless.

## See also

- Helper function
  [`VarDist()`](https://dmpstats.github.io/roamR/reference/VarDist.md)
  to construct `<VarDist>` objects.

- Package
  [distributional](https://pkg.mitchelloharawild.com/distributional/reference/distributional-package.html)
  for access to and details on a comprehensive selection of
  distributions.

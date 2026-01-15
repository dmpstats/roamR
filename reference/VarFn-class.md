# Class `<VarFn>`

`<VarFn>` is an S4 class that declares the functional properties of a
variable of interest to be used in the IBM simulation.

## Details

It enables the specification of a variable as a function of one or more
predictors through an expression, thereby describing the relationship
between the variable and its explanatory factors. This provides a
structured approach to incorporating dynamic dependencies between model
components. This class extends from the
[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)
class, inheriting its slots.

## Slots

- `fn`:

  a function, providing the functional relationship between a model
  variable and predicting factors.

- `args_spec`:

  a list of
  \<[`ArgSpec`](https://dmpstats.github.io/roamR/reference/ArgSpec-class.md)\>
  objects, one for each argument of `fn`.

- `units`:

  character string defining the output units of `fn`. Units must be
  recognized by the
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  database.

- `fn_cmp`:

  compiled version `fn`, resulting from applying e.g.
  [`build_cost_fn()`](https://dmpstats.github.io/roamR/reference/build_cost_fn.md)
  to `fn`.

## See also

Helper function
[`VarFn()`](https://dmpstats.github.io/roamR/reference/VarFn.md) to
construct `<VarFn>` objects.

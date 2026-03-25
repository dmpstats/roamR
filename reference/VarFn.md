# Create a `<VarFn>` object

Construct an instance of a
\<[`VarFn`](https://dmpstats.github.io/roamR/reference/VarFn-class.md)\>
object, which declares the functional properties of a variable of
interest to be used in the IBM simulation.

## Usage

``` r
VarFn(fn = NULL, args_spec = NULL, units = NULL, fn_cmp = NULL)
```

## Arguments

- fn:

  a function, providing the functional relationship between a model
  variable (the function's output) and its predicting factors (the
  function's arguments). **Note:** Default argument values in `fn` are
  ignored. Values for inputs must be provided via `args_spec`.

- args_spec:

  a list containing
  \<[`ArgSpec`](https://dmpstats.github.io/roamR/reference/ArgSpec-class.md)\>
  objects and/or `name = value` pairs, one for each argument of `fn`.
  See **Details** for shortcuts to facilitate the specification of
  different types of `<ArgSpec>`.

- units:

  character string defining the output units of `fn`. Units must be
  recognized by the
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  database.

- fn_cmp:

  a compiled version `fn`, resulting from applying e.g.
  [`build_cost_fn()`](https://dmpstats.github.io/roamR/reference/build_cost_fn.md)
  to `fn`.

## Details

A `<VarFn>` object declares the relationship between a response variable
and its explanatory variables, allowing to define model variables that
are dependent on the

### Shortcuts for `args_spec`

## See also

[`ArgSpec()`](https://dmpstats.github.io/roamR/reference/ArgSpec.md) for
creating `<ArgSpec>` objects.

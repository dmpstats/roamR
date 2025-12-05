# Create a `<VarFn>` object

Create a `<VarFn>` object

## Usage

``` r
VarFn(fn = NULL, args_spec = NULL, units = NULL, fn_cmp = NULL)
```

## Arguments

- args_spec:

  a list

- fn_cmp:

  compiled `fn`, resulting from applying e.g.
  [`build_cost_fn()`](https://dmpstats.github.io/roamR/reference/build_cost_fn.md)
  to `fn`

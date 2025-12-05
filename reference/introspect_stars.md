# Introspects `<stars>` objects from the `stars_obj` argument of `Driver()`

Internal helper used during the construction of `<Drivers>` objects via
[`Driver()`](https://dmpstats.github.io/roamR/reference/Driver.md) to
perform validation checks and extract dimensional metadata from the
`<stars>` object specified to `stars_obj`. The collected metadata is
required for subsequent internal model use, and is intended to be stored
in slot `@stars_meta` of `<Driver>` objects.

## Usage

``` r
introspect_stars(x, arg = rlang::caller_arg(x), call = rlang::caller_env())
```

## Arguments

- x:

  a `<stars>` object to be validated and classified.

## Value

a list with metadata extracted from `x`

## Details

**Important Note**: checks enforced here are specific to the object
construction phase within
[`Driver()`](https://dmpstats.github.io/roamR/reference/Driver.md).
Additional lower-level validation on `stars_obj` inputs is handled by
the dedicated S4 class validator.

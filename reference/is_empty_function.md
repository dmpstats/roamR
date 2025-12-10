# Assertion for empty `<function>` objects

Assertion for empty `<function>` objects

## Usage

``` r
is_empty_function(f)
```

## Arguments

- f:

  A function.

## Value

`TRUE` if the function body is empty
([`{}`](https://rdrr.io/r/base/Paren.html)), `FALSE` otherwise. An error
is raised if `f` is not a function.

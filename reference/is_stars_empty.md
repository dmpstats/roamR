# Assertion for empty `<stars>` objects

Currently `{stars}` doesn't appear to have a formalised way to define
and test empty objects. So, we re using `stars::st_as_stars(matrix(NA))`
to generate empty `<stars>`, and here we specify the function to test
the emptiness of such objects.

## Usage

``` r
is_stars_empty(x)
```

## Arguments

- x:

  an object of class `<stars>`

## Value

logical, whether x is a `<stars>` object or not

## Details

For internal use only.

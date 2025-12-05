# Slicer

A more flexible approach to the dplyr-based `stars.slice()` method,
allowing for dynamic slicing over multiple dimensions of a `<stars>`
object.

## Usage

``` r
slice_strs(strs, dim_along, ..., .drop = FALSE)
```

## Arguments

- strs:

  a `<stars>` object

- dim_along:

  a integer or character vector specifying the dimensions along which to
  slice the `<stars>` array

- ...:

  integer or character vectors providing the indices or values to slice
  for each of the dimensions specified in `dim_along`. The order must
  match the order of dimensions in `dim_along`.

- .drop:

  logical, drop dimensions that only have a single index after slicing?

## Value

A `<stars>` object containing the sliced subset of the original
multi-dimensional array.

## Details

This function enables users to extract specific slices along one or more
dimensions while optionally dropping singleton dimensions.

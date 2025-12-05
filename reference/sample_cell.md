# Sampling a density map as PDF

Sampling a density map as PDF

## Usage

``` r
sample_cell(strs, n = 1)
```

## Arguments

- n:

  How many samples to draw

- x:

  A `<stars>` object. The first attribute is used to convert to a
  probability surface, based on which sampling is performed.

## Value

A matrix of x,y coordinates, which are the centre of the sampled raster
cells

## Examples

``` r
x <- data.frame(expand.grid(x=1:5, y = 1:5), z = rlnorm(25)) |>
     stars::st_as_stars()

sample_cell(x, 10)
#> Error in sample_cell(x, 10): could not find function "sample_cell"
```

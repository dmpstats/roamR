# Store and Restore RNGs

Helpers for debugging errors during simulation, specifically:

- `save_rng()`: stores the RNG vector in current environment into an
  external file

- `restore_rng()`: restores the RNG saved in a file into the current
  global environment

## Usage

``` r
save_rng(savefile = tempfile())
```

## Arguments

- savefile:

  the path of the file storing the RNG returned by `.Random.seed`

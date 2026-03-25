# Set function environment, binding locally defined functions

\#' Creates a self-contained environment for a function by capturing its
non-package dependencies (globals) and binding them locally. This is
specifically designed for `<VarFn>` objects to ensure they are portable
and functional when dispatched to parallel workers.

## Usage

``` r
set_fn_env(fn)
```

## Arguments

- fn:

  A function to be isolated.

## Value

A copy of `fn` with a new environment containing identified local
function dependencies, with the caller environment as its parent. Find,
recursively, global objects called in function and return dependency
functions that are defined in Global environment, which are treated as
locally defined functions.

## Details

The function uses a Depth-First Search (`dfs`) to recursively find all
global objects called by `fn`. It specifically extracts functions
defined in the `.GlobalEnv` and injects them into a new self-contained
environment.

This minimizes "object not found" errors when the function is executed
in a fresh R session or on a remote cluster node.

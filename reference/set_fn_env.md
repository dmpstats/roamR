# Set function environment, binding locally defined functions

Creates a self-contained environment for a function by capturing its
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
functions that are NOT:

- primitives

- part of a package namespace

## Details

The function uses a Depth-First Search (`dfs`) to recursively find all
global objects called by `fn`. It specifically extracts functions that
are not either primitives nor part of a package namespace. Ideitified
functions are treated as user-defined functions, which are injected into
a new self-contained environment of the `fn`.

This minimizes "object not found" errors when the function is executed
on a remote cluster node.

# Validate Object Class

A internal input checker function to validate whether an object belongs
to the expected class, providing clear and informative `{cli}`-style
error messages.

## Usage

``` r
check_class(
  x,
  class,
  inlist = FALSE,
  class_fn = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env(),
  return_msg = FALSE
)
```

## Arguments

- x:

  the target object to validate. If a `list`, each element will be
  checked against the expected class

- class:

  character string, the name of the expected class of `x`.

- inlist:

  logical, whether to take `x` as a list containing objects of the
  expected `class`

- class_fn:

  character string, indicating the function to create objects of the
  expected `class`, in the form of "packagename::functionname".

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Details

This is an input checker function, i.e. it is intended for internal use
within other functions to check their inputs - it acts on behalf of
other functions as their input validator.

If `x` is a `list`, the function checks each element individually,
throwing an error if any element does not belong to the expected
`class`.

`class_fn` is optional, and is used to generate a link in the error
message to the function's manual page, if available.

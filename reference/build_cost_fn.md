# State cost function factory

Internal function operator for manufacturing functions to calculate
energetic costs associated with states based on a user-defined base
function. This function interprets the arguments of the base function,
and works out the features to construct a function that can be used in
the model. The returned manufactured function is a wrapper of the
original (base) function that can be used within the package
infrastructure. It's arguments are fixed and provide the required
objects to compute or extract the input values for the original
function.

## Usage

``` r
build_cost_fn(vrf, state_id = NULL, step_duration = NULL)
```

## Arguments

- vrf:

  an `<VarFn>` object, the user-defined function for calculating the
  energy cost of a given state. It gets wrapped by a function that is
  usable within `{roamR}`s infrastructure.

## Details

The function passed on to `base_fun` needs to observe a bunch of
requirements:

- Argument names must be the ID of a driver, "b" for biomass and/or "t"
  for time spent on activity on a current time-step

- it is be responsible to handle the intended units

# Check if units of an object are those expected under a specific context

Check if units of an object are those expected under a specific context

## Usage

``` r
check_units_contextual(
  x,
  context = c("length", "weight", "energy", "energy-time", "speed"),
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)
```

## Arguments

- x, :

  an object that inherits the class

- context:

  character, the expected unit context of the object

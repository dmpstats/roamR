# Create a `<ArgSpec>` object

Helper function to construct an instance of a
\<[`ArgSpec`](https://dmpstats.github.io/roamR/reference/ArgSpec-class.md)\>
object, which defines the metadata of a function's argument, including
its name, expected type, default value, description, and (if applicable)
its probability distribution and measurement units.

## Usage

``` r
ArgSpec(
  name,
  type = c("driver", "body_mass", "time_at_state", "constant", "random"),
  distr = NULL,
  value = NULL,
  units = NULL,
  driver_id = NULL,
  state_id = NULL,
  description = NA_character_
)
```

## Arguments

- name:

  character, the name of the argument. If an empty string (`""`), an
  empty `<ArgSpec>` object is returned, regardless of other inputs.

- type:

  character, the expected type of argument within the `{roamR}` context.
  Must be one of:

  - `"driver"`: refers to an argument linked to an existing driver.

  - `"body_mass"`: relates to the agent’s body mass.

  - `"time_at_state"`: used for arguments related to the time spent by
    the agent in a given state during the current simulation time step.

  - `"constant"`: the argument has a fixed value across simulations.

  - `"random"`: the argument is drawn from a probability distribution.

- distr:

  a `<distributional>` object describing the probability distribution of
  the argument's values. Required only if `type = "random"`.

- value:

  the fixed value assigned to the argument. Required only if
  `type = "constant"`.

- units:

  character string defining the measurement unit for the argument,
  either by name (e.g. `"grams"`) or symbol (e.g. `"m/s"`). Units must
  be recognized by the
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  database. Defaults to:

  - "grams" if `type = "body_mass"`

  - "minutes" if `type = "time_at_state"`.

- driver_id:

  character string, the ID of a driver associated with the argument
  (used when `type = "driver"`). This must match the ID of a `<Driver>`
  object available during model initialization via
  [`rmr_initiate()`](https://dmpstats.github.io/roamR/reference/rmr_initiate.md).
  If not defined, defaults to `name`. For all other types, the
  associated slot `@driver_id` is set to `NA`.

- state_id, :

  character, required if `type = "time_at_state"`; specifies the ID of
  the referred state. Assumes the existence of a `<State>` object with a
  matching ID during the IBM's initialization phase (via
  [`rmr_initiate()`](https://dmpstats.github.io/roamR/reference/rmr_initiate.md)).

- description:

  character string, a brief explanation of the argument's purpose.

## Examples

``` r
# driver ID set to `name` by default
ArgSpec("sst", "driver")
#> An object of class "ArgSpec"
#> Slot "name":
#> [1] "sst"
#> 
#> Slot "type":
#> [1] "driver"
#> 
#> Slot "value":
#> NULL
#> 
#> Slot "driver_id":
#> [1] "sst"
#> 
#> Slot "state_id":
#> [1] NA
#> 
#> Slot "description":
#> [1] NA
#> 
#> Slot "distr":
#> <distribution[1]>
#> [1] NA
#> 
#> Slot "units":
#> [1] ""
#> 

# linking argument name to a given driver ID
ArgSpec("x", "driver", driver_id = "sst", units = "Degrees_celsius")
#> An object of class "ArgSpec"
#> Slot "name":
#> [1] "x"
#> 
#> Slot "type":
#> [1] "driver"
#> 
#> Slot "value":
#> NULL
#> 
#> Slot "driver_id":
#> [1] "sst"
#> 
#> Slot "state_id":
#> [1] NA
#> 
#> Slot "description":
#> [1] NA
#> 
#> Slot "distr":
#> <distribution[1]>
#> [1] NA
#> 
#> Slot "units":
#> [1] "Degrees_celsius"
#> 

# argument referring to agents' body mass, in kilograms
ArgSpec("b", "body_mass", units = "kg")
#> An object of class "ArgSpec"
#> Slot "name":
#> [1] "b"
#> 
#> Slot "type":
#> [1] "body_mass"
#> 
#> Slot "value":
#> NULL
#> 
#> Slot "driver_id":
#> [1] NA
#> 
#> Slot "state_id":
#> [1] NA
#> 
#> Slot "description":
#> [1] NA
#> 
#> Slot "distr":
#> <distribution[1]>
#> [1] NA
#> 
#> Slot "units":
#> [1] "kg"
#> 

# argument whose input values follow a Bernoulli distribution
ArgSpec("x", "random", distr = distributional::dist_bernoulli(0.1), units = "m")
#> An object of class "ArgSpec"
#> Slot "name":
#> [1] "x"
#> 
#> Slot "type":
#> [1] "random"
#> 
#> Slot "value":
#> NULL
#> 
#> Slot "driver_id":
#> [1] NA
#> 
#> Slot "state_id":
#> [1] NA
#> 
#> Slot "description":
#> [1] NA
#> 
#> Slot "distr":
#> <distribution[1]>
#> [1] Bernoulli(0.1)
#> 
#> Slot "units":
#> [1] "m"
#> 

```

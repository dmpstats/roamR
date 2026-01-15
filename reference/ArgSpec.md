# Create a `<ArgSpec>` object

Helper function to construct an instance of a
\<[`ArgSpec`](https://dmpstats.github.io/roamR/reference/ArgSpec-class.md)\>
object, which defines the properties of a function's argument, including
its name, default value, probability distribution, measurement units,
etc.

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

  character, the expected type of argument within roamR's IBM context.
  Must be one of:

  - `"driver"`: An argument linked to an existing driver; its value is
    determined by the agent's status (e.g. location) at the current
    simulation step.

  - `"body_mass"`: Refers to the agent’s body mass at the current
    simulation step.

  - `"time_at_state"`: An argument related to time spent by the agent in
    a given state during the current simulation time step.

  - `"constant"`: The argument has a fixed value across simulations.

  - `"random"`: The argument is stochastic, i.e. it's value is dictated
    by a probability distribution.

- distr:

  a `<distributional>` object describing the probability distribution of
  the argument's values. Required only if `type = "random"`.

- value:

  the fixed value assigned to the argument. Required only if \`type =
  "constant".

- units:

  character string defining the measurement unit for the argument within
  the function, either by name (e.g. `"grams"`) or abbreviation (e.g.
  `"m/s"`). Units must be recognized by the
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  database. Defaults to:

  - "grams" if `type = "body_mass"`

  - "minutes" if `type = "time_at_state"`.

- driver_id:

  character string, the ID of a driver associated with the argument
  (used when `type = "driver"`). This must match the ID of a `<Driver>`
  object available during model initialization via
  [`rmr_initiate()`](https://dmpstats.github.io/roamR/reference/rmr_initiate.md).
  If not defined, defaults to `name`. For all other `type`s, the
  associated slot `@driver_id` is set to `NA`.

- state_id, :

  character, required if `type = "time_at_state"`; specifies the ID of
  the referred state. Assumes the existence of a `<State>` object with a
  matching ID during the IBM's initialization stage (via
  [`rmr_initiate()`](https://dmpstats.github.io/roamR/reference/rmr_initiate.md)).

- description:

  character string, a brief explanation of the argument's purpose.

## Examples

``` r
# driver ID set to `name` by default
ArgSpec("sst", "driver")
#> <ArgSpec>
#> `sst`: driver "sst" evaluated at current step

# linking argument name to a given driver ID
ArgSpec("x", "driver", driver_id = "sst", units = "Degrees_celsius")
#> <ArgSpec>
#> `x`: driver "sst" evaluated at current step [°C]

# argument referring to agents' body mass, in kilograms
ArgSpec("b", "body_mass", units = "kg")
#> <ArgSpec>
#> `b`: agent's body mass at current step [kg]

# argument whose input values follow a Bernoulli distribution
ArgSpec("x", "random", distr = distributional::dist_bernoulli(0.1), units = "m")
#> <ArgSpec>
#> `x`: Bernoulli(0.1) [m]

```

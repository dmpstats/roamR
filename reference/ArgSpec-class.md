# Class `<ArgSpec>`

An S4 class for specifying the characteristics of a function argument.

## Details

`<ArgSpec>` defines the metadata of a function's argument, including its
name, expected type, default value, description, and (if applicable) its
probability distribution and measurement units.

At a lower level,`<ArgSpec>` interacts with the
[`VarFn`](https://dmpstats.github.io/roamR/reference/VarFn-class.md)
class to define input parameters for a user-defined function. At a
higher level, it supports `{roamR}`'s **IBM definition** by linking
functions arguments to the broader simulation infrastructure.

## Slots

- `name`:

  character, the name of the argument.

- `type`:

  character, the expected type of argument within the `{roamR}` context.
  Must be one of:

  - `"driver"`: refers to an argument linked to an existing driver.

  - `"body_mass"`: relates to the agent’s body mass.

  - `"time_at_state"`: used for arguments related to the time spent by
    the agent in a given state during the current simulation time step.

  - `"constant"`: the argument has a fixed value across simulations.

  - `"random"`: the argument is drawn from a probability distribution.

- `value`:

  the value assigned to the argument.

- `driver_id`:

  character, required if `type = "driver"`; specifies the ID assigned to
  a given driver. This assumes the existence of a `<Driver>` object with
  a matching ID during the IBM's initialization phase (via
  [`rmr_initiate()`](https://dmpstats.github.io/roamR/reference/rmr_initiate.md)).
  Failing that, initialization will not be successful.

- `state_id,`:

  character, required if `type = "time_at_state"`; specifies the ID of
  the referred state. Assumes the existence of a `<State>` object with a
  matching ID during the IBM's initialization phase (via
  [`rmr_initiate()`](https://dmpstats.github.io/roamR/reference/rmr_initiate.md)).

- `description`:

  character string, a brief explanation of the argument's purpose.

- `distr`:

  Inherited from parent class
  \<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>,
  an object of class
  [`<distribution>`](https://pkg.mitchelloharawild.com/distributional/reference/distributional-package.html).
  Required if `type = "random"`, representing the probability
  distribution associated with the argument's value.

- `units`:

  Inherited from parent class
  \<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>.
  A character string defining the measurement unit for the argument,
  either as a name (e.g. `"grams"`) or a symbol (e.g. `"m/s"`). Units
  must be recognized by the
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  database.

## See also

Helper function
[`ArgSpec()`](https://dmpstats.github.io/roamR/reference/ArgSpec.md) to
construct `<ArgSpec>` objects

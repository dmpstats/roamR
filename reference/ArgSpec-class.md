# Class `<ArgSpec>`

An S4 class for specifying the characteristics of a function argument.

## Details

`<ArgSpec>` defines the properties of a function's argument, such as its
name, type, value or probability distribution, measurement units and
general description.

At a lower level,`<ArgSpec>` interacts with the
[`VarFn`](https://dmpstats.github.io/roamR/reference/VarFn-class.md)
class to define parameters for user-specified function that determine
the value of a model variable at each simulation iteration. At a higher
level, it ensures model robustness linking function arguments to the
IBM's simulation infrastructure.

## Slots

- `name`:

  character, the name of the argument.

- `type`:

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

  - `"random"`: The argument is stochastic and it's value is dictated by
    a probability distribution.

- `value`:

  the fixed value assigned to the argument. Required only if
  `type = "constant"`.

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

# Class `<State>`

An S4 class representing the properties of an agent's behavioural or
activity state. Each `<State>` object defines characteristics such as
energy expenditure, time allocation, and movement speed at the
individual level.

## Details

`<State>` is designed as a sub-class of
[Species](https://dmpstats.github.io/roamR/reference/Species-class.md),
allowing a species' state profile to be structured as a list of
`<State>` objects. This flexible design enables customized state
specifications for different species.

Currently, three state attributes are supported: energy cost, time
budget, and movement speed. However, the class design allows for future
expansions to accommodate additional state attributes.

## Slots

- `id`:

  character string, a unique identifier for the state, representing a
  specific behaviour or activity.

- `energy_cost`:

  a
  \<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>
  object, defining the energy expenditure associated with the state
  (e.g. kJ/hour/grams).

- `time_budget`:

  a
  \<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>
  object, defining the agent's typical time allocation to this state. It
  should be expressed as a relative length of time (e.g. hours/day).

- `speed`:

  a
  \<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>
  object, specifying the movement speed associated with this state (e.g.
  m/s).

## See also

- [`VarDist()`](https://dmpstats.github.io/roamR/reference/VarDist.md)
  for defining `<VarDist>` objects

- Helper function
  [`State()`](https://dmpstats.github.io/roamR/reference/State.md) to
  construct `<State>` objects

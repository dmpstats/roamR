# Create a `<State>` object

Helper function to construct instances of
\<[`State`](https://dmpstats.github.io/roamR/reference/State-class.md)\>
objects, enabling the specification of attributes specific to an agent's
behavioural or activity state. Each `<State>` object defines
characteristics such as energy expenditure, time allocation, and
movement speed at the individual level.

## Usage

``` r
State(
  id = NA_character_,
  energy_cost = VarDist(),
  time_budget = VarDist(),
  speed = VarDist()
)
```

## Arguments

- id:

  character string, a unique identifier for the state, representing a
  specific behaviour or activity.

- energy_cost:

  a
  \<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>
  or a
  \<[`VarFn`](https://dmpstats.github.io/roamR/reference/VarFn-class.md)\>
  object, defining the energy expenditure associated with the state
  (e.g. kJ/hour/grams).

- time_budget:

  a
  \<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>
  object, defining the agent's typical time allocation to this state. It
  should be expressed as a relative length of time (e.g. hours/day).

- speed:

  a
  \<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>
  object, specifying the movement speed associated with this state (e.g.
  m/s).

## Value

a
\<[`State`](https://dmpstats.github.io/roamR/reference/State-class.md)\>
S4 object

## See also

[`VarDist()`](https://dmpstats.github.io/roamR/reference/VarDist.md) for
defining `<VarDist>` objects

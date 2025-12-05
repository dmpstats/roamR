# Create `<AgentCondition>` objects

`AgentCondition()` is a helper function to construct instances of
`[AgentCondition-class]` objects, which carries the status of an agent
at the end of a simulation `delta_time` (e.g. end of day).

## Usage

``` r
AgentCondition(
  location = sf::st_point(),
  grid_cell = sf::st_point(),
  timestep = NA_integer_,
  timestamp = as.Date(NA),
  body_mass = NA_real_,
  states_budget = list(),
  states_cost = list(),
  mortality_prob = NA_real_,
  alive = NA,
  track = sf::st_sf(timestamp = as.POSIXct(NA), geom = sf::st_sfc(sf::st_point()))
)
```

## Arguments

- location:

  object of class `XY`, specifying the agent's spatial coordinates at
  the end of the current time-step.

- grid_cell:

  object of class `XY`, representing the AOC's grid-cell occupied by the
  agent.

- timestep:

  integer, indicating the current simulation time-step index.

- timestamp:

  a `<POSIXct>` object, the date-time at the end of the current
  time-step.

- body_mass:

  a `<units>` object, defining the agent's body mass at the end of the
  current time-step.

- states_budget:

  a named list, detailing the agent's time allocation across the
  different behavioural/activity states during the time-step. Each
  element (of type `<units>`) is named after a specific state and stores
  the duration (e.g. hours) the agent spent in that state.

- states_cost:

  a named list, specifying the energy costs (e.g. KJ/hr) associated with
  each state defined in `state_budget`. List elements are of type
  `<units>`.

- mortality_prob:

  numeric, the probability of the agent dying within the the current
  time-step (*currently ignored*).

- alive:

  a logical value (`TRUE` if alive, `FALSE` if dead), indicating whether
  the agent has survived the time-step.

- track:

  object of class `sf`, storing a time-series of location points
  tracking the agent's movement within the current time-step.

- energy_expenditure:

  a `<units>` object, representing the total energy (e.g. KJ/g) expended
  across all states during the time-step.

- foraging_success:

  a `<units>` object, the mass of prey (e.g. grams) consumed by the
  agent in the current time-step.

- mass_change_value:

  a `<units>` object, capturing the net change in the agent’s body mass
  (e.g., grams) over the current time-step.

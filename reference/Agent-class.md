# `<Agent>`

`<Agent>` is an S4 class encapsulating the biological and physiological
properties and historical status of an individual agent within the
simulated IBM. It uses classes
\<[AgentCondition](https://dmpstats.github.io/roamR/reference/AgentCondition-class.md)\>
and
\<[AgentProperties](https://dmpstats.github.io/roamR/reference/AgentProperties-class.md)\>,
also collating a historical record of the agent's movement and state
properties generated throughout the simulation.

## Slots

- `condition`:

  object of class
  \<[AgentCondition](https://dmpstats.github.io/roamR/reference/AgentCondition-class.md)\>,
  describing the agent's condition at the end of the current simulation
  time-step.

- `properties`:

  object of class
  \<[AgentProperties](https://dmpstats.github.io/roamR/reference/AgentProperties-class.md)\>,
  defining attributes of the agent that remain constant throughout
  simulation.

- `history`:

  object of class `sf`, storing historical data comprising the agent's
  state and spatio-temporal activity for each time-step of the
  simulation.

## See also

- Helper function
  [`Agent()`](https://dmpstats.github.io/roamR/reference/Agent.md) to
  construct `<Agent>` objects

- Helper functions
  [`AgentProperties()`](https://dmpstats.github.io/roamR/reference/AgentProperties.md)
  and
  [`AgentCondition()`](https://dmpstats.github.io/roamR/reference/AgentCondition.md)

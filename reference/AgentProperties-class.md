# `<AgentProperties>`

`<AgentProperties>` is an S4 class specifying the individual-level
properties of an agent being traced in the simulation model. It subsumes
features that remain constant throughout the simulation run, including
biological traits, average travelling speeds, initialization and final
locations, susceptibility of movement and states to model drivers.

Currently this is an "internal" class, i.e. not meant to be user-facing.

## Slots

- `species_id`:

  character, the identifier code for the agent's species.

- `initial_mass`:

  `<units>` object, the agent's body mass at the start of the
  simulation.

- `speeds`:

  a named list, defining movement speed properties for the agent. Each
  element specifies the agent's average speed for a given movement state
  (e.g. flying, swimming, etc). List elements must be of type `<units>`.

- `cost_par_draws`:

  a named list bla

- `start_point,end_point`:

  objects of class `XY`, the spatial coordinates of the agent at the
  start and end of the simulation, respectively. the agent is assumed to
  die.

- `mortality_thresh`:

  `<units>` object, the threshold body mass below which the agent is
  assumed to die.

- `energy_to_mass`:

  a `<units>` object, providing the agent's energy-to-bodymass
  conversion rate (e.g. g/kJ).

- `move_influences`:

  a named list, defining whether the agent is influenced by model
  drivers. Each element corresponds to a `driver_id` (which must be
  defined in the model's
  [Driver](https://dmpstats.github.io/roamR/reference/Driver-class.md)
  object) and contains a single-row `data.frame` with columns:

  - `p`: numeric, providing the probability that the agent is influenced
    by the named driver.

  - `infl`: logical, whether the agent is influenced by the driver. It
    should be determined via a Bernoulli trial with probability `p`.

- `state_influences`:

  a named list, defining whether agent states are influenced by model
  drivers. Each element, named after a `driver_id`, contains a
  `data.frame` with columns:

  - `state_id`: character string, the unique identifier of an agent's
    state.

  - `p`: numeric, the probability that the agent's `state_id` is
    influenced by the driver.

  - `infl`: logical, whether the agent's `state_id` is influenced by the
    driver. It should be determined via a Bernoulli trial with
    probability `p`.

  - `ext`: `<units>` value, the extent of the influence on the agent's
    `state_id`.

- `age`:

  `<units>` object, the age of the agent at the start of the simulation
  (currently unused).

- `sex`:

  character, the sex of the agent, where `"f"`denotes female and `"m"`
  denotes male (currently unused).

## See also

Helper function
[`AgentProperties()`](https://dmpstats.github.io/roamR/reference/AgentProperties.md)
to create `<AgentProperties>` objects

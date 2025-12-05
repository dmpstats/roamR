# Create `<AgentProperties>` objects

`AgentProperties()` is a helper function to construct instances of
[AgentProperties](https://dmpstats.github.io/roamR/reference/AgentProperties-class.md)
objects. This function initializes an agent's properties, including body
mass, movement parameters, and interactions with environmental drivers.

## Usage

``` r
AgentProperties(
  species_id = NA_character_,
  initial_mass = NULL,
  speeds = list(),
  cost_par_draws = list(),
  start_point = sf::st_point(),
  end_point = sf::st_point(),
  mortality_thresh = NULL,
  energy_to_mass = NULL,
  move_influences = list(),
  state_influences = list(),
  age = NULL,
  sex = c("f", "m"),
  ...,
  species = NULL,
  model_config = NULL
)
```

## Arguments

- species_id:

  character, the identifier code for the agent's species.

- initial_mass:

  `<units>` object, the agent's body mass at the start of the
  simulation.

- speeds:

  a named list, defining movement speed properties for the agent. Each
  element specifies the agent's average speed for a given movement state
  (e.g. flying, swimming, etc). List elements must be of type `<units>`.

- cost_par_draws:

  a named list bla

- start_point, end_point:

  objects of class `XY`, the spatial coordinates of the agent at the
  start and end of the simulation, respectively.

- mortality_thresh:

  `<units>` object, the threshold body mass below which the agent is
  assumed to die.

- energy_to_mass:

  a `<units>` object, providing the agent's energy-to-bodymass
  conversion rate (e.g. g/kJ).

- move_influences:

  a named list, defining whether the agent is influenced by model
  drivers. Each element corresponds to a `driver_id` (which must be
  defined in the model's
  [Driver](https://dmpstats.github.io/roamR/reference/Driver-class.md)
  object) and contains a single-row `data.frame` with columns:

  - `p`: numeric, providing the probability that the agent is influenced
    by the named driver.

  - `infl`: logical, whether the agent is influenced by the driver. It
    should be determined via a Bernoulli trial with probability `p`.

- state_influences:

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

- age:

  `<units>` object, the age of the agent at the start of the simulation
  (currently unused).

- sex:

  character, the sex of the agent, where `"f"`denotes female and `"m"`
  denotes male (currently unused).

- ...:

  reserved for future extensions (currently ignored).

- species:

  a
  \<[Species](https://dmpstats.github.io/roamR/reference/Species-class.md)\>
  object, If not `NULL` (default), it is used to populate the slots of
  the returned `<AgentProperties>` object, overriding the above function
  arguments.

- model_config:

  a
  [ModelConfig](https://dmpstats.github.io/roamR/reference/ModelConfig-class.md)
  object, used alongside Species to populate the returned
  `<AgentProperties>` object.

## Value

an object of class
[AgentProperties](https://dmpstats.github.io/roamR/reference/AgentProperties-class.md)

## Details

If `species` (and `model_config`) are specified, all other arguments are
ignored. The
[AgentProperties](https://dmpstats.github.io/roamR/reference/AgentProperties-class.md)
object is then populated using the provided `<Species>` and
`<ModelConfig>` objects. Specifically:

- `@initial_mass` is sampled from `Species@body_mass_distr`

- `@speeds` are generated based on `Species@behaviour_profile`

- `@mortality_thresh` is sampled from `Species@mortality_thresh_distr`

- `@start_point` and `@end_point` are defined from
  `ModelConfig@start_sites` and `ModelConfig@end_sites`. In cases where
  these slots are empty, the agent's start/end locations are randomly
  assigned within the AOC area based on slot `ModelConfig@aoc_bbox`.

- `@move_influences` and `state_influences` are generated from
  `Species@driver_responses`.

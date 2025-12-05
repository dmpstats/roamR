# `<Species>`

`<Species>` is an S4 class that comprises species-level properties of
simulated agents. It includes attributes related to taxonomy,
physiology, behavioural/activity states, responses to environmental and
non-environmental drivers, and population size.

## Slots

- `id`:

  character string, a unique identifier for the species.

- `common_name`:

  character string, the common name of the species.

- `scientific_name`:

  character string, the scientific name of the species.

- `body_mass_distr`:

  an object of class
  [VarDist](https://dmpstats.github.io/roamR/reference/VarDist-class.md),
  defining the distribution of body mass for the species.

- `energy_to_mass_distr`:

  a
  [VarDist](https://dmpstats.github.io/roamR/reference/VarDist-class.md)
  object, providing the energy-to-bodymass conversion rate for the
  species (e.g. g/kJ). Determines how gained or lost energy translates
  into changes in body mass.

- `states_profile`:

  a list of
  [State](https://dmpstats.github.io/roamR/reference/State-class.md)
  objects, defining the behavioural/activity states for the species to
  consider in the model.

- `driver_responses`:

  list of
  [DriverResponse](https://dmpstats.github.io/roamR/reference/DriverResponse-class.md)
  objects, specifying the species’ responses to predefined model
  drivers, such as environmental pressures, biological influences, and
  human-induced impacts affecting movement and behaviour/activity
  states. See
  [DriverResponse](https://dmpstats.github.io/roamR/reference/DriverResponse-class.md)
  for details.

  Each element in the `driver_responses` list must correspond to an
  existing object of class
  [Driver](https://dmpstats.github.io/roamR/reference/Driver-class.md).
  At the initialization phase of the Individual-Based Model (IBM), these
  `driver_responses` are matched with their respective
  [Driver](https://dmpstats.github.io/roamR/reference/Driver-class.md)
  objects to ensure a consistent and well-defined set of species-driver
  interactions. Failure to provide a corresponding
  [Driver](https://dmpstats.github.io/roamR/reference/Driver-class.md)
  object for each entry of `driver_responses` will result in missing or
  undefined driver effects during the simulation.

- `mortality_thresh_distr`:

  an object of class
  [VarDist](https://dmpstats.github.io/roamR/reference/VarDist-class.md),
  specifying the values of agent's condition (e.g. body mass), below
  which it is assumed to have died (*Note: slot may require further
  generalization*)

- `pop_size`:

  numeric, the population size (i.e., number of individuals) within the
  area of calculation (AOC).

## See also

- Helper function
  [`Species()`](https://dmpstats.github.io/roamR/reference/Species.md)
  to define `<Species>` objects

- Helper functions
  [`VarDist()`](https://dmpstats.github.io/roamR/reference/VarDist.md),
  [`DriverResponse()`](https://dmpstats.github.io/roamR/reference/DriverResponse.md)
  and [`State()`](https://dmpstats.github.io/roamR/reference/State.md)
  for constructing objects of the dependency classes

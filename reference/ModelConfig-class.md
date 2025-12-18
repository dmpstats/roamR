# Class `<ModelConfig>`

`<ModelConfig>` is an S4 class containing the user-defined configuration
for the Individual-based Model (IBM). It specifies key parameters such
as the number of agents, the spatio-temporal resolution of the model,
the bounding box (extent) of the area of calculation (AOC), the
simulation start and end date.

## Slots

- `movement_type`:

  Character string, specifying the movement model to simulate agent
  trajectories. Currently supported options:

  - `"di"`: Density-informed movement, where agent destinations
    stochastically generated based on population-level density maps.

  - `"crw"`: Correlated Random Walk model, where movement directions
    follow probability distributions that can be conditioned by habitat,
    environmental and/or physiological factors.

- `n_agents`:

  Integer, the number of agents to track within the simulation.

- `ref_sys`:

  Object of class \<`crs`\>, defining the Coordinate Reference System to
  be applied to the IBM. Must be specified via
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).

- `aoc_bbx`:

  Object of class \<`bbox`\>, specifying the area of calculation (AOC),
  i.e. the spatial bounding box within which simulation occurs.

- `delta_x,delta_y`:

  Numeric, specifying the cell (pixel) size in the x and y dimensions,
  respectively. These define the spatial resolution of the AOC. Required
  only when `movement_type = "crw"`; otherwise values are automatically
  derived from the resolution of provided density maps. Units are
  assumed to match those of `ref_sys`.

- `delta_time`:

  Character string, defines the temporal resolution of the model. Valid
  options include "hours", "day", "week", "month" or "year", and can be
  preceded by a positive integer and a space, and followed by "s".

- `start_date;end_date`:

  Date, respectively, defines the start and end dates for the simulation
  period.

- `start_sites`:

  An `<sf>` object, defining the sites where agents start the
  simulation. Apart from the sites' geometry, this object must contain
  the following columns:

  - `id`: a unique identifier for each site.

  - `prop`: the proportion of `n_agents` allocated at each site. The
    values in this column must sum to 1.

  If `start_sites` are not provided, agents start at random locations
  within the AOC.

- `end_sites`:

  An `<sf>` object, analogous to `start_sites`, but specifying the sites
  to which agents must return to at the end of the simulation. If `NULL`
  (the default), end locations are not forced upon agent. **Note:** This
  parameter is currently inactive and will be ignored.

## See also

[`ModelConfig()`](https://dmpstats.github.io/roamR/reference/ModelConfig.md)
for creating `<ModelConfig>` objects and further details on the
specification of input values.

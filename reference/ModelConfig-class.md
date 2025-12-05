# Class `<ModelConfig>`

`<ModelConfig>` is an S4 class containing the user-defined configuration
for the Individual-based Model (IBM). t specifies key parameters such as
the number of agents, the spatio-temporal resolution of the model, the
bounding box (extent) of the area of calculation (AOC), the simulation
start and end date.

## Details

### `start_sites` and `end_sites`

For site geometries other than points, agents' starting/end locations
are randomly drawn within the boundary of the geometry. For example, if
a site is defined by polygon(s), agents assigned to that site start/end
at random points within the polygon.

## Slots

- `n_agents`:

  integer, the number of agents to track within the simulation.

- `ref_sys`:

  object of class \<`crs`\>, defining the Coordinate Reference System to
  be applied to the IBM. Must be specified via
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).

- `aoc_bbx`:

  object of class \<`bbox`\>, specifying the area of calculation, i.e.
  the spatial bounding box within which simulation occurs.

- `delta_x,delta_y`:

  numeric, the cell (pixel) size in the x and y dimensions,
  respectively. Assumed to take the same units as `ref_sys`.

- `delta_time`:

  character string, defines the temporal resolution of the model. Must
  be one of "day", "week", "month", "quarter" or "year", and can
  optionally be preceded by a positive integer and a space, and followed
  by "s".

- `start_date;end_date`:

  Date, respectively, defines the start and end dates for the simulation
  period.

- `start_sites`:

  an `<sf>` object, defining the sites where agents start the
  simulation. Apart from the sites' geometry, this object must contain
  the following columns:

  - `id`: a unique identifier for each site.

  - `prop`: the proportion of `n_agents` allocated at each site. The
    values in this column must sum to 1.

  If `start_sites` are not provided, agents are assigned to random
  locations within the AOC.

- `end_sites`:

  an `<sf>` object, analogous to `start_sites`, but specifying the sites
  to which agents must return to at the end of the simulation. If `NULL`
  (the default), end locations are not forced upon agent. **Note:** This
  parameter is currently inactive and will be ignored.

## See also

Helper function
[`ModelConfig()`](https://dmpstats.github.io/roamR/reference/ModelConfig.md)
for constructing `<ModelConfig>` objects

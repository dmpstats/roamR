# Class `<Driver>`

`<Driver>` is an S4 class representing spatially explicit environmental
or anthropogenic features that interact with simulated agents. These
drivers influence agents’ movement, behavioural states, energetic
expenditure, and physiological condition within the IBM's area of
calculation (AOC) over the course of a simulation.

## Details

The class supports four main categories of drivers:

- `"habitat"` - represents baseline environmental or anthropogenic
  conditions that define habitat characteristics (e.g. Sea surface
  temperature, salinity, bathymetry, distance from land).

- `"resource"`: captures the spatial distribution and availability of
  resources or proxies of ecological suitability (e.g. species density
  maps, energy intake surfaces).

- `"disturbance"` - refers to influencers that deviate from the
  status-quo and evaluated for their effects on the agents' simulated
  histories (e.g. offshore windfarm footprints, oil rigs, shipping
  corridors).

- `"model"` - refers to influencers specified for model operational
  purposes (e.g. AOC bounding box to confine simulated movements).

`<Driver>` supports two types of spatial data: vector-based simple
features (`<sf>`) and gridded spatio-temporal data cubes (`<stars>`),
providing flexibility for representing diverse inputs.

This class interacts with the
[`Species`](https://dmpstats.github.io/roamR/reference/Species-class.md)
to define how agents respond to drivers, such as modifying movement
patterns or altering behavioural energy costs.

## Slots

- `id`:

  character string, a unique identifier for the driver.

- `descr`:

  descr character string, providing a general description of the driver.

- `ann`:

  character string, annotation for referencing the driver for reporting
  purposes.

- `type`:

  character string, the driver type within the context of the IBM.
  Currently supports one of two values: "habitat" or "impact".

- `sf_obj`:

  object of class `<sf>`, the simple feature representing the spatial
  geometry of the driver, if applicable.

- `sf_descr`:

  character string, a brief description of data contained in `sf_obj`.

- `stars_obj`:

  an object of class `<stars>`, a multidimensional array containing
  grid-type spatio-temporal attributes of the driver. Typical examples
  include time-series of raster-type density surfaces, distance layers
  to landscape features or to man-made structures/impacts.

- `stars_meta`:

  a list object containing metadata about `stars_obj` required for
  modelling purposes

- `stars_descr`:

  character string, a brief description of the data contained in
  `stars_obj`.

- `obj_active`:

  character string, flagging whether `sf_obj` or `stars_obj` is to be
  used as the active driver data during simulation.

## See also

Helper function
[`Driver()`](https://dmpstats.github.io/roamR/reference/Driver.md) to
create `<Driver>` objects.

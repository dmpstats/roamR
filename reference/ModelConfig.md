# Create a `<ModelConfig>` object

Helper function to define the model configuration of the IMB. It
constructs instances of
\<[`ModelConfig`](https://dmpstats.github.io/roamR/reference/ModelConfig-class.md)\>
objects.

## Usage

``` r
ModelConfig(
  n_agents = 100L,
  ref_sys = sf::st_crs(4326),
  aoc_bbx = c(0, 0, 10, 10),
  delta_x = 0.25,
  delta_y = 0.25,
  delta_time = "1 day",
  start_date = Sys.Date() - 5,
  end_date = Sys.Date(),
  start_sites = NULL,
  end_sites = NULL
)
```

## Arguments

- n_agents:

  integer, the number of agents to track within the simulation.

- ref_sys:

  object of class \<`crs`\>, defining the Coordinate Reference System to
  be applied to the IBM. Must be specified via
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html).

- aoc_bbx:

  numeric vector or object of class \<`bbox`\>, specifying the area of
  calculation, i.e. the spatial bounding box within which simulation
  occurs. If numeric, expects a 4-length vector specifying `xmin`,
  `ymin`, `xmax` and `ymax` values.

- delta_x, delta_y:

  numeric, the cell (pixel) size in the x and y dimensions,
  respectively. Assumed to take the same units as `ref_sys`.

- delta_time:

  character string, defines the temporal resolution of the model. Must
  be one of "day", "week", "month", "quarter" or "year", and can
  optionally be preceded by a positive integer and a space, and followed
  by "s".

- start_sites:

  an `<sf>` object, defining the sites where agents start the
  simulation. Apart from the sites' geometry, this object must contain
  the following columns:

  - `id`: a unique identifier for each site.

  - `prop`: the proportion of `n_agents` allocated at each site. The
    values in this column must sum to 1.

  If `NULL` (the default), agents start at random locations within the
  AOC.

- end_sites:

  an `<sf>` object, analogous to `start_sites`, specifying the sites to
  which agents must return to at the end of the simulation. If `NULL`
  (the default), end locations are not forced upon agent. **Note:** This
  parameter is currently inactive and will be ignored.

- start_date;end_date:

  Date, respectively, defines the start and end dates for the simulation
  period.

## Value

An object of class
\<[ModelConfig](https://dmpstats.github.io/roamR/reference/ModelConfig-class.md)\>

## Details

### `start_sites` and `end_sites`

- For site geometries other than points, agents' starting/end locations
  are randomly drawn within the boundary of the geometry. For example,
  if a site is defined by polygon(s), agents assigned to that site
  start/end at random points within the polygon.

## Examples

``` r
library(sf)
library(ggplot2)

# specify colonies
colonies <- st_sf(
  id = c("A", "B", "C"),
  prop = c(0.30, 0.30, 0.40),
  geom = st_sfc(st_point(c(1,1)), st_point(c(2,2)), st_point(c(3,3))),
  crs = 4326
)

# initialize model configuration object
config <- ModelConfig(
  n_agents = 1000,
  ref_sys = st_crs(4326),
  aoc_bbx = c(0, 0, 5, 5),
  delta_x = 0.25,
  delta_y = 0.25,
  delta_time = "1 day",
  start_date = as.Date("2022-09-01"),
  end_date = as.Date("2022-09-30"),
  start_sites = colonies,
  end_sites = colonies
)

# Accessors
aoc_bbx(config)
#> xmin ymin xmax ymax 
#>    0    0    5    5 
start_sites(config)
#> Simple feature collection with 3 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 3 ymax: 3
#> Geodetic CRS:  WGS 84
#>   id prop        geom
#> 1  A  0.3 POINT (1 1)
#> 2  B  0.3 POINT (2 2)
#> 3  C  0.4 POINT (3 3)
end_sites(config)
#> Simple feature collection with 3 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 3 ymax: 3
#> Geodetic CRS:  WGS 84
#>   id prop        geom
#> 1  A  0.3 POINT (1 1)
#> 2  B  0.3 POINT (2 2)
#> 3  C  0.4 POINT (3 3)

# vizualizing the AOC's bounding box, the start and end sites
ggplot() +
  geom_sf(data = st_as_sfc(aoc_bbx(config)), col = "orange", fill = NA) +
  geom_sf(data = start_sites(config), size = 4, colour = "darkgreen") +
  geom_sf(data = end_sites(config), col = "red")

```

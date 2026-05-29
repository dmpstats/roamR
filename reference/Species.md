# Create `<Species>` objects

Helper function to construct instances of
[Species](https://dmpstats.github.io/roamR/reference/Species-class.md)
objects, representing species-level properties of simulated agents.
These include attributes related to taxonomy, physiology,
behavioural/activity states, responses to environmental and
non-environmental drivers, and population size.

## Usage

``` r
Species(
  id = NA_character_,
  common_name = NA_character_,
  scientific_name = NA_character_,
  body_mass_distr = VarDist(),
  energy_to_mass_distr = VarDist(),
  mortality_thresh_distr = VarDist(),
  states_profile = list(),
  driver_responses = list(),
  pop_size = NA_real_
)
```

## Arguments

- id:

  character string, a unique identifier for the species.

- common_name:

  character string, the common name of the species.

- scientific_name:

  character string, the scientific name of the species.

- body_mass_distr:

  an object of class
  [VarDist](https://dmpstats.github.io/roamR/reference/VarDist-class.md),
  defining the distribution of body mass for the species.

- energy_to_mass_distr:

  a
  [VarDist](https://dmpstats.github.io/roamR/reference/VarDist-class.md)
  object, providing the energy-to-bodymass conversion rate for the
  species (e.g. g/kJ). Determines how gained or lost energy translates
  into changes in body mass.

- mortality_thresh_distr:

  an object of class
  [VarDist](https://dmpstats.github.io/roamR/reference/VarDist-class.md),
  specifying the values of agent's condition (e.g. body mass), below
  which it is assumed to have died (*Note: parameter may require further
  generalization*)

- states_profile:

  a list of
  [State](https://dmpstats.github.io/roamR/reference/State-class.md)
  objects, defining the behavioural/activity states for the species to
  consider in the model.

- driver_responses:

  a list of
  [DriverResponse](https://dmpstats.github.io/roamR/reference/DriverResponse-class.md)
  objects, specifying the species’ responses to predefined model
  drivers, such as environmental pressures, biological influences, and
  human-induced impacts affecting movement and behaviour/activity
  states. See the Details section below and
  [DriverResponse](https://dmpstats.github.io/roamR/reference/DriverResponse-class.md)
  for further information.

- pop_size:

  numeric, the population size (i.e., number of individuals) within the
  area of calculation (AOC).

## Details

Each element in the `driver_responses` list must correspond to an
existing object of class
[Driver](https://dmpstats.github.io/roamR/reference/Driver-class.md). At
the initialization phase of `{roamR}`s IBM, these `driver_responses` are
matched with their respective
[Driver](https://dmpstats.github.io/roamR/reference/Driver-class.md)
objects to ensure a consistent set of species-driver interactions.
Failure to provide a corresponding
[Driver](https://dmpstats.github.io/roamR/reference/Driver-class.md)
object for each entry of `driver_responses` will result in missing or
undefined driver effects during the simulation.

## See also

Helper functions
[`VarDist()`](https://dmpstats.github.io/roamR/reference/VarDist.md),
[`DriverResponse()`](https://dmpstats.github.io/roamR/reference/DriverResponse.md)
and [`State()`](https://dmpstats.github.io/roamR/reference/State.md)

## Examples

``` r

library(distributional)
#> 
#> Attaching package: ‘distributional’
#> The following objects are masked from ‘package:roamR’:
#> 
#>     generate, parameters

# 1. Start by setting the state profile
states <- list(
 flight = State(
    id = "flying",
    energy_cost = VarDist(dist_uniform(1, 2), units = "kJ/hour/gram"),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
    speed = VarDist(dist_uniform(10, 20), "m/s")
    ),
 dive = State(
    id = "diving",
    energy_cost = VarDist(dist_uniform(1, 2), units = "kJ/hour/gram"),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
    speed = VarDist(dist_uniform(10, 20), "m/s")
  )
)

# 2. Then the list of driver responses
driver_resp <- list(
  coast = DriverResponse(
    driver_id = "land",
    movement = MoveInfluence(
      prob = VarDist(distributional::dist_degenerate(1)),
      fn = function(x, slope = 1/2) exp(-slope * x),
      type = "repulsion"
    )),
  owf = DriverResponse(
    driver_id = "owf_foot",
    movement = MoveInfluence(
      prob = VarDist(dist_beta(2, 8)),
      fn = function(x, slope = 1/2) exp(-slope * x),
      type = "repulsion"
    ),
    states = list(
      StateInfluence(
        state_id = "flying",
        prob = VarDist(dist_beta(1, 5)),
        extent = VarDist(dist_lognormal(2, 1), "hr/day")
      )))
  )

# 3. specify <Species>
guill <- Species(
     id = "guill",
     common_name = "guillemot",
     scientific_name = "Uria Aalge",
     pop_size = 10000,
     body_mass_distr = VarDist(dist_uniform(850, 1130), "g"),
     mortality_thresh_distr = VarDist(dist_normal(500, 20), "g"),
     states_profile = states,
     driver_responses = driver_resp
   )
```

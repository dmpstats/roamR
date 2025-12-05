# Get element of list of S4 objects

Helper to get element of list of S4 objects using it's slot `@id`.
Applicable to e.g. `IBM@drivers` or `Species@states_profile`

## Usage

``` r
pluck_s4(l, id)
```

## Arguments

- l:

  a list containing S4 class objects as elements, each of which must
  contain a slot @id

- id:

  character, the name assigned to @id

## Examples

``` r
pluck_s4(rover@states_profile, "water_resting")
#> Error in pluck_s4(rover@states_profile, "water_resting"): could not find function "pluck_s4"

pluck_s4(rover_ibm_disnbs@drivers, "dens")
#> Error in pluck_s4(rover_ibm_disnbs@drivers, "dens"): could not find function "pluck_s4"

# returns NULL if there is no element with specified ID
pluck_s4(rover_ibm_disnbs@drivers, "water_resting")
#> Error in pluck_s4(rover_ibm_disnbs@drivers, "water_resting"): could not find function "pluck_s4"
```

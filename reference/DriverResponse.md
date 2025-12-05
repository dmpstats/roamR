# Create a `<DriverResponse>` object

Helper function to construct instances of
\<[`DriverResponse`](https://dmpstats.github.io/roamR/reference/DriverResponse-class.md)\>
objects, defining per-agent responses to a specific environmental or
man-made "driver", as declared in an associated object of class
\<[`Driver`](https://dmpstats.github.io/roamR/reference/Driver-class.md)\>.

## Usage

``` r
DriverResponse(
  driver_id = NA_character_,
  movement = MoveInfluence(),
  states = list(),
  condition = NA_character_
)
```

## Arguments

- driver_id:

  character string, the unique identifier of the driver. It must match
  the `@id` value in the concomitant
  \<[`Driver`](https://dmpstats.github.io/roamR/reference/Driver-class.md)\>
  object. Unmatched IDs will trigger an error during model
  initialization.

- movement:

  an object of class
  \<[`MoveInfluence`](https://dmpstats.github.io/roamR/reference/MoveInfluence-class.md)\>,
  specifying how the driver influences the agent's movement

- states:

  a list of
  \<[`StateInfluence`](https://dmpstats.github.io/roamR/reference/StateInfluence-class.md)\>
  objects, listing the influence of the driver in the agent's states.

- condition:

  a character string (feature under development). Reserved for
  specifying additional effects of the driver on agent condition.

## Details

For model initialization, a `<DriverResponse>` object must be linked to
its corresponding
\<[`Driver`](https://dmpstats.github.io/roamR/reference/Driver-class.md)\>
object via the `@driver_id` slot in `<DriverResponse>` and the `@id`
slot in
\<[`Driver`](https://dmpstats.github.io/roamR/reference/Driver-class.md)\>.
Unmatched IDs will result in an error during the IBM's initialization
phase.

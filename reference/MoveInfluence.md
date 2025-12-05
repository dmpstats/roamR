# Create a `<MoveInfluence>` object

Helper function to construct instances of
\<[`MoveInfluence`](https://dmpstats.github.io/roamR/reference/MoveInfluence-class.md)\>
objects

## Usage

``` r
MoveInfluence(
  prob = VarDist(),
  fn = function(x) 0,
  type = c("null", "attraction", "repulsion"),
  mode = c("vector-field", "cell-value"),
  sim_stage = c("bsln", "imp", "bsln-imp")
)
```

## Arguments

- prob:

  an object of class
  \<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>,
  expressing the probability, and associated uncertainty/variability,
  that an agent's movement will be influenced by a driver.

- fn:

  a `function`, defining the influence function applied to modify agent
  movement based on the driver's spatial properties. Specifically, this
  function describes the relationship between the magnitude of the
  influence and the spatial rate of change of the driver. The function
  should take a single argument, expressing the driver's rate of change,
  and its output, expressing the magnitude of the influence, should be
  bounded between 0 (no influence) and 1 (maximum influence).

- type:

  A `character` string, specifying the type of influence being applied
  (e.g., "attraction", "repulsion"). This allows for categorization of
  movement influence.

- mode:

  A `character` string, specifying how the driver is to be applied by
  the movement model. Currently supports 2 options:

  - "vector-field" (default): TODO

  - "cell-value": TODO

- sim_stage:

  A `character` string, specifying at which stage of the simulation is
  driver applied by the movement model. Currently supports three
  options:

  - "bsln" (default): TODO

  - "imp": TODO

  - "bsln-imp": TODO

# `<MoveInfluence>`

`<MoveInfluence>` is an S4 class for defining the influence of a given
driver on the movement of simulated agents. This class is intended to be
used as a sub-class of
\<[`DriverResponse`](https://dmpstats.github.io/roamR/reference/DriverResponse-class.md)\>\`

## Details

This class allows specifying the probabilistic influence of a driver on
agent movement, the function governing the movement influence, and the
type of influence being modelled.

## Slots

- `prob`:

  an object of class
  \<[`VarDist`](https://dmpstats.github.io/roamR/reference/VarDist-class.md)\>,
  expressing the probability, and associated uncertainty/variability,
  that an agent's movement will be influenced by a driver.

- `fn`:

  a `function`, defining the influence function applied to modify agent
  movement based on the driver's spatial properties. Specifically, this
  function describes the relationship between the magnitude of the
  influence and the spatial rate of change of the driver. The function
  should take a single argument, expressing the driver's rate of change,
  and its output, expressing the magnitude of the influence, should be
  bounded between 0 (no influence) and 1 (maximum influence).

- `type`:

  A `character` string, specifying the type of influence being applied
  (e.g., "attraction", "repulsion"). This allows for categorization of
  movement influence.

- `mode`:

  A `character` string, specifying how the driver is to be applied by
  the movement model. Currently supports 2 options:

  - "vector-field" (default): TODO

  - "cell-value": TODO

- `sim_stage`:

  A `character` string, specifying at which stage of the simulation is
  driver applied by the movement model. Currently supports three
  options:

  - "bsln" (default): TODO

  - "imp": TODO

  - "bsln-imp": TODO

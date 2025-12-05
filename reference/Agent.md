# Create `<Agent>` objects

`Agent()` is a helper function for constructing instances of
[Agent](https://dmpstats.github.io/roamR/reference/Agent-class.md)
objects. It relies on predefined classes `<Species>` and `<ModelConfig>`
objects to initiate its slots accordingly.

## Usage

``` r
Agent(species = NULL, model_config = NULL)
```

## Arguments

- species:

  object of class
  \<[Species](https://dmpstats.github.io/roamR/reference/Species-class.md)\>,
  specifying the agent's species-level properties. If `NULL` (default),
  species-related slots in `<Agent>` are initialized as empty.

- model_config:

  object of class
  \<[ModelConfig](https://dmpstats.github.io/roamR/reference/ModelConfig-class.md)\>,
  defining the IBM's configuration. If `NULL` (default), model-related
  slots in `<Agent>` are initialized as empty.

## See also

- Helper functions
  [`Species()`](https://dmpstats.github.io/roamR/reference/Species.md)
  and
  [`ModelConfig()`](https://dmpstats.github.io/roamR/reference/ModelConfig.md)

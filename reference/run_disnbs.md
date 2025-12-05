# Run the DisNBS individual-based model

Run the model developed for the DisNB simulation. This approach is based
on a movement model where tracks are simulated based on density
distribution maps

## Usage

``` r
run_disnbs(
  ibm,
  run_scen = c("baseline", "impact", "baseline-and-impact"),
  dens_id,
  intake_id = NULL,
  imp_dens_id = NULL,
  imp_intake_id = NULL,
  feed_state_id,
  roost_state_id,
  feed_avg_net_energy = units::set_units(422, "kJ/h"),
  target_energy = units::set_units(1, "kJ"),
  smooth_body_mass = bm_smooth_opts(),
  waypnts_res = 100,
  seed = sample(3000, 1),
  quiet = FALSE
)
```

## Arguments

- ibm:

  an object of class `<IMB>`. Use function
  [`rmr_initiate()`](https://dmpstats.github.io/roamR/reference/rmr_initiate.md)
  to create the required object.

- dens_id:

  character string, the identifier for the `<Driver>` object containing
  the agent's baseline density map. This must always be provided, even
  when `run_scenario` is set to `"impact"`, since impacted movement is
  dependent on baseline movement patterns. See the **Details** section
  for more information.

- intake_id:

  character string, the identifier for the `<Driver>` object containing
  the agent's baseline energy intake map.

- imp_dens_id:

  character string, the identifier for the `<Driver>` object containing
  the agent's impacted density map.

- imp_intake_id:

  character string, the identifier for the `<Driver>` object containing
  the agent's impacted energy intake map.

- feed_avg_net_energy:

  a `<units>` object, specifying the average net energy per unit of
  feeding time. This is a tuning parameter expressing the energy
  required to balance the energetic equations for an average agent.

- target_energy:

  a `<units>` object, the target daily net energy. Constitutes the
  objective value used in the agent's state rebalancing process in terms
  of daily energy. This controls the extent agents change their feeding
  behaviour in response to feeding success - low success means more
  feeding the following day.

- smooth_body_mass:

  an object of class `<bm_smooth_opts>`, specifying options for
  converting energy time-series to body mass. Use
  [`bm_smooth_opts()`](https://dmpstats.github.io/roamR/reference/bm_smooth_opts.md)
  to define these options.

- waypnts_res:

  the distance between waypoints defining the movement path of simulated
  agents. Either a `<numeric>` value, expressed in meters, or an
  `<units>` object with a valid length units.

- seed:

  integer, the random seed to use in do simulation, for reproducibility
  purposes.

- quiet:

  logical. If `TRUE`, suppresses messages and progress output during the
  simulation.

- run_scenario:

  a character string defining which scenario(s) to run in the
  simulation. Options are:

  - `"baseline"` (default): simulate using data representing the
    status-quo scenario.

  - `"impact"`: simulate using data representing the impact scenario.

  - `"baseline-and-impact"`: sequentially simulate both baseline and
    impact scenarios.

## Details

(expand dependency of impacted scenario on baseline density surface)

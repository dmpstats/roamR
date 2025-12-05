# Run Agent-level simulation under the DisNBS model

Executes an individual-based simulation for a single `<Agent>` object
under the DisNBS model framework. This function models the agent's
spatial movement and physiological condition over the simulation period.
Movement is based on density maps using the shortest path method. Energy
intake maps are used for balancing out energy expenditure.

## Usage

``` r
simulate_agent_disnbs(
  agent,
  drivers,
  states_profile,
  scen = c("baseline", "impact"),
  night_proportion,
  dnbs_cfg,
  feed_avg_net_energy,
  target_energy = units::set_units(1, "kJ")
)
```

## Arguments

- agent:

  `<Agent>`, representing the individual to be simulated.

- dnbs_cfg:

  TODO

- dens:

  `<stars>`, providing the spatio-temporal species density distribution
  used to drive the agent's movement.

- intake_drv:

  `<stars>`, containing the energy intake surface for the agent. Used to
  simulate energetic dynamics.

## Value

A modified `<Agent>` object containing the complete simulated trajectory
and condition history of the agent over the simulation period, based on
the provided inputs.

## Details

Notes to expand:

- Assumes movement is exclusively one-directional, towards the end of
  each track

- once agent gets to a track's endpoint, it stays there until the
  following rerouting step, relardless of of the travelling distance
  returned from the states

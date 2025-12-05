# Rebalance activity states

Rebalance activity states

## Usage

``` r
rebalance_states(
  states_budget,
  night_prop,
  feed_state_id,
  roost_state_id,
  curr_energy,
  feed_avg_net_energy,
  target_energy,
  step_duration
)
```

## Arguments

- states_budget:

  A list of time budget allocated to each state (relative proportion)

- night_prop:

  Proportion of day that is night (roost state cannot fall below this)

- feed_state_id:

  ID of the feeding state, providing energy intake

- roost_state_id:

  ID of the roosting state (where night constraint applies)

- curr_energy:

  net energy level at the current time-step (kJ)

- feed_avg_net_energy:

  average net energy intake per unit of time feeding (e.g. kJ/hr)

- target_energy:

  cumulative net energy target (kJ)

- step_duration:

  duration of the simulation time-step (e.g 1 day)

## Value

A list of time budgets allocated to each state (as relative proportions)

NOTES:

- Only applicable to diurnal species

## Examples

``` r
TBD
#> Error: object 'TBD' not found
```

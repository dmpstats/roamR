# Getting Started

*Page Under Construction*

``` r
library(roamR)
```

## Introduction

This article provides a fast-track nahds-on demonstration of how to
build an Individual-Based Model (IBM) using roamR. We’ll walk through a
mock example featuring an imaginary population in a hypothetical habitat
to illustrate the core workflow. In this example, we’ll use the
**Density-informed** movement model.

The aim is to help you quickly familiarise yourself with the package’s
main functions, object classes, and features, while providing a
practical reference for future implementations. For a more detailed
description of roamR’s components, see the [roamR
Overview](https://dmpstats.github.io/roamR/articles/articles/roamR-guide.md)
article.

## General roamR workflow

The standard workflow to construct and run an IMB in roamR can be
summarised in the following steps:

1.  **High-level model configuration** - Set the spatial and temporal
    boundaries and resolutions, number of agents, and other
    simulation-level parameters.

2.  **Specify species features** - Define attributes such as initial
    body mass, flight speed, activity states and their energetic
    trade-offs, and interactions with environment.

3.  **Define the environment** - Include relevant spatio-temporal layers
    within the simulated area such as species distributions, coastlines,
    man-made structures, environmental attributes, etc.

4.  **Combine components into an IBM object** - Integrate the species
    and environment definitions into a single simulation-ready object.

5.  **Run the simulation** - Create agents, parallelise computations,
    move agents through the environment, and record their properties
    over time - optionally including defined perturbation scenarios.

6.  **Query simulated animals** - Investigate results, e.g. by comparing
    individual responses across baseline and impact scenarios.

In the following sections, we’ll go over each of these steps and build
our own IBM from scratch. Buckle up!

## The *Turtwick* IBM: a baseline scenario

In this demonstrative example, we’ll focus on a hypothetical species,
the *Turtwick*, which inhabits a grassland area expected to be altered
by the introduction of man-made structures. From previous observations,
we know that *turtwicks* tend to avoid such structures, often showing
displacement behaviour in response to their presence.

We also know that the species’ body mass and activity behaviours (here
referred to as **states**) are influenced by surface temperature and
vegetation index. *turtwicks* are diurnal: individuals remain completely
inactive at night, resting in any nearby, reasonably safe location where
they stop by the end of the day.

### 1. Model Configuration

We start by defining the high-level configurations of the IBM for our
*turtwick* population using the
[`ModelConfig()`](https://dmpstats.github.io/roamR/reference/ModelConfig.md)
function:

``` r
turtwick_ibm_cfg <- ModelConfig(
  n_agents = 100,
  ref_sys = sf::st_crs(32630), 
  aoc_bbx = c(0, 0, 10000, 10000), 
  delta_x = 250, 
  delta_y = 250, 
  delta_time = "1 day", 
  start_date = as.Date("2025-01-01"), 
  end_date = as.Date("2025-02-01")
)
```

In plain terms, this configuration defines a model in which we:

- track the movement and life history of **100** *turtwicks*
  (`n_agents`)
- use the **UTM 30N** coordinate reference system (`ref_sys`)
- constrain movement within a **10x10 km** area of calculation with
  origin at **(0,0)**, using a spatial resolution of **250x250 m** cells
  (`delta_x` and `delta_y`)
- simulate the population from 1 January to 1 February 2025, in daily
  time steps (`delta_time`, `start_date`, `end_date`)

You can inspect the configuration details by calling the created
`turtwick_ibm_cfg` object:

``` r
turtwick_ibm_cfg
#> <ModelConfig> instance with attributes:
#> • Movement Model:      Density-informed
#> • No. Agents:          100
#> • Simulation period:   2025-01-01 -- 2025-02-01 (31 days)
#> • Temporal resolution: 1 day
#> • Bounding box:        xmin: 0  ymin: 0  xmax: 10000  ymax: 10000 [m]
#> • Projected CRS:       WGS 84 / UTM zone 30N
#> • Start site:          NA
#> • End site:            NA
```

### 2. Species Definition

babababa bbababa maldodsdf sjaf kfodhf OHDFOH FOhf ofh ohfdhf odhfo
godhgo goshg \[ojf j\] pdsgj pgjpjgpdojg pjg

### 2. Environment Specification

babababa bbababa maldodsdf sjaf kfodhf OHDFOH FOhf ofh ohfdhf odhfo
godhgo goshg \[ojf j\] pdsgj pgjpjgpdojg pjg

## Disturbing *Turtwegs*: baseline Vs impact scenarios

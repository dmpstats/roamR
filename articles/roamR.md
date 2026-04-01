# Getting Started

*Page Under Construction*

``` r
library(roamR)
```

## Introduction

This article provides a fast-track hands-on demonstration of how to
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
    yyry yryyr ooro individual responses across baseline and impact
    scenarios.

In the following sections, we’ll go over each of these steps and build
our own IBM from scratch. Buckle up!

## The *Turtwick* IBM: the baseline scenario

In this example, we focus on a hypothetical species, the *Turtwick*,
which inhabits grassland areas expected to be altered by the
introduction of man-made structures. Previous observations indicate that
*turtwicks* tend to avoid such structures, often showing displacement
behaviour in response to their presence.

We also know that the species’ body mass and activity behaviours (here
referred to as **states**) are influenced by surface temperature and
vegetation index. *Turtwicks* are diurnal: individuals remain completely
inactive at night, resting in a nearby safe location once the sun sets.

### 1. Model Configuration

We start by defining the high-level configurations of the IBM for our
*turtwick* population using the
[`ModelConfig()`](https://dmpstats.github.io/roamR/reference/ModelConfig.md)
function:

``` r
turtwick_ibm_cfg <- ModelConfig(
  movement_model = "di",
  n_agents = 100,
  ref_sys = sf::st_crs(32630), 
  aoc_bbx = c(0, 0, 10000, 10000), 
  delta_time = "1 day", 
  start_date = as.Date("2025-01-01"), 
  end_date = as.Date("2025-02-01")
)

turtwick_ibm_cfg
## <ModelConfig> instance with attributes:
## • Movement Model:      Density-informed
## • No. Agents:          100
## • Simulation period:   2025-01-01 -- 2025-02-01 (31 days)
## • Temporal resolution: 1 day
## • Bounding box:        xmin: 0  ymin: 0  xmax: 10000  ymax: 10000 [m]
## • Projected CRS:       WGS 84 / UTM zone 30N
## • Start site:          NA
## • End site:            NA
```

In plain terms, this configuration sets up a model where we:

- Simulate the movement and life history of **100** *turtwicks*
  (`n_agents`) from 1 January to 1 February 2025, using daily time steps
  (`delta_time`, `start_date`, `end_date`).

- Use the **UTM 30N** coordinate reference system (`ref_sys`).

- Restrict movement to a **10x10 km** area of calculation (AOC) with its
  origin at **(0,0)**, using a spatial resolution of **250x250 m** cells
  (`delta_x` and `delta_y`)

- Initialise agents at random locations within the AOC, allowing them to
  end up anywhere inside this area by the end of the simulation.

### 2. Species Definition

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis sagittis
posuere ligula sit amet lacinia. Duis dignissim pellentesque magna,
rhoncus congue sapien finibus mollis. Ut eu sem laoreet, vehicula ipsum
in, convallis erat. Vestibulum magna sem, blandit pulvinar augue sit
amet, auctor malesuada sapien. Nullam faucibus leo eget eros hendrerit,
non laoreet ipsum lacinia. Curabitur cursus diam elit, non tempus ante
volutpat a. Quisque hendrerit blandit purus non fringilla. Integer sit
amet elit viverra ante dapibus semper. Vestibulum viverra rutrum enim,
at luctus enim posuere eu. Orci varius natoque penatibus et magnis dis
parturient montes, nascetur ridiculus mus.

Nunc ac dignissim magna. Vestibulum vitae egestas elit. Proin feugiat
leo quis ante condimentum, eu ornare mauris feugiat. Pellentesque
habitant morbi tristique senectus et netus et malesuada fames ac turpis
egestas. Mauris cursus laoreet ex, dignissim bibendum est posuere
iaculis. Suspendisse et maximus elit. In fringilla gravida ornare.
Aenean id lectus pulvinar, sagittis felis nec, rutrum risus. Nam vel
neque eu arcu blandit fringilla et in quam. Aliquam luctus est sit amet
vestibulum eleifend. Phasellus elementum sagittis molestie. Proin tempor
lorem arcu, at condimentum purus volutpat eu. Fusce et pellentesque
ligula. Pellentesque id tellus at erat luctus fringilla. Suspendisse
potenti.

Etiam maximus accumsan gravida. Maecenas at nunc dignissim, euismod enim
ac, bibendum ipsum. Maecenas vehicula velit in nisl aliquet ultricies.
Nam eget massa interdum, maximus arcu vel, pretium erat. Maecenas sit
amet tempor purus, vitae aliquet nunc. Vivamus cursus urna velit,
eleifend dictum magna laoreet ut. Duis eu erat mollis, blandit magna id,
tincidunt ipsum. Integer massa nibh, commodo eu ex vel, venenatis
efficitur ligula. Integer convallis lacus elit, maximus eleifend lacus
ornare ac. Vestibulum scelerisque viverra urna id lacinia. Vestibulum
ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia
curae; Aenean eget enim at diam bibendum tincidunt eu non purus. Nullam
id magna ultrices, sodales metus viverra, tempus turpis.

### 2. Environment Specification

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis sagittis
posuere ligula sit amet lacinia. Duis dignissim pellentesque magna,
rhoncus congue sapien finibus mollis. Ut eu sem laoreet, vehicula ipsum
in, convallis erat. Vestibulum magna sem, blandit pulvinar augue sit
amet, auctor malesuada sapien. Nullam faucibus leo eget eros hendrerit,
non laoreet ipsum lacinia. Curabitur cursus diam elit, non tempus ante
volutpat a. Quisque hendrerit blandit purus non fringilla. Integer sit
amet elit viverra ante dapibus semper. Vestibulum viverra rutrum enim,
at luctus enim posuere eu. Orci varius natoque penatibus et magnis dis
parturient montes, nascetur ridiculus mus.

Nunc ac dignissim magna. Vestibulum vitae egestas elit. Proin feugiat
leo quis ante condimentum, eu ornare mauris feugiat. Pellentesque
habitant morbi tristique senectus et netus et malesuada fames ac turpis
egestas. Mauris cursus laoreet ex, dignissim bibendum est posuere
iaculis. Suspendisse et maximus elit. In fringilla gravida ornare.
Aenean id lectus pulvinar, sagittis felis nec, rutrum risus. Nam vel
neque eu arcu blandit fringilla et in quam. Aliquam luctus est sit amet
vestibulum eleifend. Phasellus elementum sagittis molestie. Proin tempor
lorem arcu, at condimentum purus volutpat eu. Fusce et pellentesque
ligula. Pellentesque id tellus at erat luctus fringilla. Suspendisse
potenti.

## Disturbing the *Turtwegs*: baseline Vs impact scenarios

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis sagittis
posuere ligula sit amet lacinia. Duis dignissim pellentesque magna,
rhoncus congue sapien finibus mollis. Ut eu sem laoreet, vehicula ipsum
in, convallis erat. Vestibulum magna sem, blandit pulvinar augue sit
amet, auctor malesuada sapien. Nullam faucibus leo eget eros hendrerit,
non laoreet ipsum lacinia. Curabitur cursus diam elit, non tempus ante
volutpat a. Quisque hendrerit blandit purus non fringilla. Integer sit
amet elit viverra ante dapibus semper. Vestibulum viverra rutrum enim,
at luctus enim posuere eu. Orci varius natoque penatibus et magnis dis
parturient montes, nascetur ridiculus mus.

Nunc ac dignissim magna. Vestibulum vitae egestas elit. Proin feugiat
leo quis ante condimentum, eu ornare mauris feugiat. Pellentesque
habitant morbi tristique senectus et netus et malesuada fames ac turpis
egestas. Mauris cursus laoreet ex, dignissim bibendum est posuere
iaculis. Suspendisse et maximus elit. In fringilla gravida ornare.
Aenean id lectus pulvinar, sagittis felis nec, rutrum risus. Nam vel
neque eu arcu blandit fringilla et in quam. Aliquam luctus est sit amet
vestibulum eleifend. Phasellus elementum sagittis molestie. Proin tempor
lorem arcu, at condimentum purus volutpat eu. Fusce et pellentesque
ligula. Pellentesque id tellus at erat luctus fringilla. Suspendisse
potenti.

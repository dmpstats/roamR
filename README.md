
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Welcome to the `roamR` package

<!-- badges: start -->

<!-- badges: end -->

`roamR` is an R package that offers a toolset and framework for running
Individual Based Models (IBMs - or Agent-Based Models). It was created
as part of the DisNBS project (Distribution of seabirds in the
Non-Breeding Season) which was a project within the Off-shore Renewables
Joint Industry Partnership programme
([ORJIP](http://www.orjip.org.uk/)). While the project has specific
goals in relation to Off-shore Wind Farms (OWF) and their potential
effects on seabirds, the problem is quite general. This package can help
build animal movement simulations, with the general problem broken down
into logical components and imposes a level of transparent formality,
such that the computational components are on a common footing, leaving
focus on parametrisation and inputs/data.

`roamR` is written in S4 and has been structured/implemented such that
the simulations are well defined, but with a lot of flexibility in what
the components can do. Animal IBMs are spatio-temporal and can be based
on very disparate data sources - so `roamR` is particular in its
treatment of space (projections etc), time and units generally (as found
in the `units` package). The package was written with animals in mind
(indeed one of the main classes is `Species`), but we’ll use the
agnostic term **agent**.

In brief - `roamR`:

- defines the environment the animal interacts with, and how this
  functionally affects its movement or behaviour
- defines how animals move, with consideration of the environment,
  behavoural state, or condition
- defines the behaviours animals may engage in and how these change
- defines the general characteristics of a type of agent (a *species*
  definition)
- creates many agents with stochastic properties in line with their
  species definition
- runs the agents through the simulated environment over time, for a
  defined temporal resolution
- measures the agents throughout the simulation
- gives tools to query the mass of agents that have been simulated

A principal limitation in IBMs is the level of information you have
about how the agents behave. `roamR` is intended to work with very
little information, up to very information rich. The test cases here
cover a range of data support:

- Relatively data rich: Guillemots at the Isle of May, which are well
  studied.
- Relatively data poor: Red-Throated Divers in the North Sea, which are
  less understood.

In the more extreme, `roamR` has been developed with the intention of
simulating very well known animal populations, for example a population
with extensive GPS tagging information, that might suffice for
estimation of a Hidden Markov Model. For example those demonstrated in
Michelot XXX fitted to Fulmar, which could be subsequently simulated in
the momentHMM package.

## Installation

`roamR` is not currently on CRAN - installation is direct from github.
You can use devtools to install:

``` r

devtools::install_github("dmpstats/roamR", ref = "master")
```

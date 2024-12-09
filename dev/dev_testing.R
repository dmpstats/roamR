BehaviourSpec(behav = "flight", energy_cost = distributional::dist_normal(1, 0), time_budget = distributional::dist_normal(3, 1))

library(distributional)
library(stars)

data.frame(expand.grid(x=1:5, y = 1:5), z = rnorm(25)) |> st_as_stars() |> plot()


# density surfaces ----------------------------
rvr_dens <- data.frame(
  expand.grid(x = 1:50, y = 1:50, month = c(9, 10, 11, 12, 1, 2), iter = as.integer(1:25)),
  counts = rpois(50*50*6*25, 2)) |>
  st_as_stars(dims = c("x","y", "month", "iter"))



rvr_dens |>
  dplyr::filter(iter == 10) |>
  plot()

# behaviour profile ------------------------------------------
behav <- list(
 flight = BehaviourSpec(
    behav = "flying",
    energy_cost = VarDist(dist_uniform(1, 2), units = "kJ/hour/gram"),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
    speed = VarDist(dist_uniform(10, 20), "m/s")
    ),
 dive = BehaviourSpec(
    behav = "diving",
    energy_cost = VarDist(dist_uniform(1, 2), units = "kJ/hour/gram"),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day")
  ),
 swimming = BehaviourSpec(
   behav = "swimming",
   energy_cost = VarDist(dist_uniform(1, 2), units = "kJ/hour/gram"),
   time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
   speed = VarDist(dist_uniform(0, 2), "m/s")
 ),
 water_rest = BehaviourSpec(
   behav = "water_resting",
   energy_cost = VarDist(dist_uniform(1, 2), units = "kJ/hour/gram"),
   time_budget = VarDist(dist_uniform(1, 3), "hours/day")
 )
)




rover <- Species(
  id = "rvr",
  role = "agent",
  common_name = "Rover",
  scientific_name = "Rover Vulgaris",
  body_mass_distr = VarDist(dist_normal(1000, 0.2 * 1000), units = "grams"),
  mortality_thresh_distr = VarDist(dist_uniform(300, 350), units = "grams"),
  spatial_distr = rvr_dens,
  behaviour_profile = list(
    x = 2)

)








hist(generate(dist_normal(1000, sd = 0.2 * 1000), 1000)[[1]])
dist <- dist_multivariate_normal(mu = list(c(1,2), c(2,2)), sigma = list(matrix(c(4,2,2,3), ncol=2)))
dimnames(dist) <- c("x", "y")
dist
generate(dist, 10)[[1]] |> plot()





rmr_initiate(
  species = new("Species"),
  habitat = Habitat(),
  structures = list(s = Structure()),
  config = 4
)




hist(rnorm(1000), plot = FALSE)



spatial_dim <- st_sf(
  ID = 1:3,
  geometry = list(
    st_polygon(list(
      cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
    )),
    st_polygon(list(
      cbind(c(1, 2, 2, 1, 1), c(0, 0, 1, 1, 0))
    )),
    st_polygon(list(
      cbind(c(2, 3, 3, 2, 2), c(0, 0, 1, 1, 0))
    ))
  )
)
weekdays_dim <- data.frame(weekdays = c("Monday", "Tuesday", "Wednesday",
                                        "Thursday", "Friday", "Saturday", "Sunday"))
hours_dim <- data.frame(hours = c("8am", "11am", "4pm", "11pm"))
sf_dta <- spatial_dim |>
  dplyr::cross_join(weekdays_dim)|>
  dplyr::cross_join(hours_dim) |>
  dplyr::mutate(population = rnorm(dplyr::n(), mean = 1000, sd = 200)) |>
  dplyr::select(everything(), geometry)

st_as_stars(sf_dta, dims = c("weekdays", "hours", "geometry"))





data <- data.frame(x = rnorm(200), y = rnorm(200))

#library(MASS)
a <- data$x
b <- data$y
f1 <- MASS::kde2d(a, b, n = 100)
filled.contour(f1)

dim(f1$z)


data.frame(expand.grid(x=1:5, y = 1:5), z = rnorm(25)) |> st_as_stars()







dist <- distributional::dist_logistic(location = c(5,9,9,6,2), scale = c(2,3,4,2,1))

dist
mean(dist)
lapply(distributional::generate(dist, 5000), mean)


y <- distributional::dist_normal(mu = 2, sigma = 2)
class(y)
plot(y)

sloop::otype(distributional::dist_beta(2, 4))


x
plot(x)


setOldClass("distribution")

methods::setClass(
  "TestClass",
  slots = list(
    speed = "distribution"
  )
)



test <- new("TestClass", speed = distributional::dist_binomial(10, 0.1))

distributional::support(test@speed)


x <- distributional::dist_normal(3, 0.1)
units::set_units(distributional::generate(x, c(5))[[1]], "m")

attributes(x)
attr(x, "units") <- "g"
x
attributes(x)



RangedNumeric <- setClass(
  "RangedNumeric",
  contains = "numeric",
  slots = c(min = "numeric", max = "numeric"),
  prototype = structure(numeric(), min = NA_real_, max = NA_real_)
)
rn <- RangedNumeric(1:10, min = 1, max = 10)
class(rn)
inherits(rn, "numeric")
str(rn)
length(rn)
mean(rn)



MyDist <- setClass(
  "MyDist",
  contains = "distribution",
  slots = c(units = "units"),
  prototype = structure(distributional::dist_missing(), units = NA)
)


test <- MyDist(distributional::dist_normal(1, 1), units = units::make_units(m))

str(test)
is(test, "distribution")
distributional::generate(test, times = 2)



VarDist(units = "mtttae")






MyDist_2 <- setClass(
  "MyDist_2",
  slots = c(
    dist = "distribution",
    units = "units"
  ),
  prototype = c(
    dist = dist_missing(),
    units = NA
  )
)


test <- MyDist_2(
  dist = distributional::dist_normal(1, 2),
  units = units::make_units(m))

distributional::generate(test@dist, 10) |> purrr::map(~as_units(.x, units(test@units)))


BehaviourSpec(
  energy_cost = VarDist(dist = distributional::dist_degenerate(1, 2), units = "m"))

# define distribution of variable based on a random sample (e.g a bootstrap)
x <- rlnorm(1000, 9, 1)
mass <- VarDist(dist = dist_sample(list(x)), c("hg", "re"))
# resample 100 values
distributional::generate(mass@dist, times = 100) |>
  lapply(units::make_units, value = mass@units)


class(mass@units)

x1 <- units::set_units(double(), "m")
x1

set_units(2, value = x1)
#
#
#
#
# with_own_units_errors <- function(expr, call = rlang::caller_env()) {
#   rlang::try_fetch(
#     expr,
#     error = function(cnd) {
#       msg <- conditionMessage(cnd)
#       if(grepl("not recognized by udunits.", msg)){
#         cli::cli_abort(c("wrong units"), call= call, parent = NA)
#       }
#     }
#   )
# }
#
# with_own_units_errors(as_units("trs"))
#
#
#
#
# grepl("not recognized by udunits.", "In ‘rrwrw’, ‘rrwrw’is not recognized by udunits.")
#
#
#
# units::as_units(1, mass@units)
#
# units::set_units(2, "") + set_units(2, "m")
#
# as.symbol()


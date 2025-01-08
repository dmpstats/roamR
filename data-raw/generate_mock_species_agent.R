## code to prepare `generate_mock_species_data` dataset goes here

### Creating a <Species> object for a mock species ('Rover') for testing and
### demonstration purposes



#### Build the species behaviour profile  ----------------------------------
rvr_behav <- list(
  flight = BehaviourSpec(
    behav = "flying",
    energy_cost = VarDist(dist_uniform(2, 2), units = "kJ/hour/gram"),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
    speed = VarDist(dist_uniform(10, 20), "m/s")
  ),
  dive = BehaviourSpec(
    behav = "diving",
    energy_cost = VarDist(dist_uniform(3, 5), units = "kJ/hour/gram"),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day")
  ),
  swimming = BehaviourSpec(
    behav = "swimming",
    energy_cost = VarDist(dist_uniform(3, 6), units = "kJ/hour/gram"),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
    speed = VarDist(dist_uniform(0, 2), "m/s")
  ),
  water_rest = BehaviourSpec(
    behav = "water_resting",
    energy_cost = VarDist(dist_uniform(0.5, 1.5), units = "kJ/hour/gram"),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day")
  )
)



#### Build the list of impact responses  -------------------------------------

rvr_imp_resp <- list(
  owf_footprint = ImpactResponse(
    impact_id = "owf_foot",
    displace_prob = VarDist(dist_beta(2, 8)),
    displace_fn = function(x, y){ x/y },
    disturb_prob = VarDist(dist_beta(1, 5)),
    disturb_behav = "flying",
    # extra hours/day of flight due to impact
    disturb_extent = VarDist(dist_lognormal(2, 1), "hr/day")
  )
)


#### Create density surface  -------------------------------------------------
library(dplyr)
library(tidyr)
library(MetBrewer)

# generate spatial grid
dns_srf_grd <- expand_grid(
  x = seq(-3, 3, by = 0.1),
  y = seq(52, 58, by = 0.1)
) |> as.matrix()

plot(dns_srf_grd)

# set monts and nr of samples
month <- month.abb[1:4]
n_samples <- 5

#set.seed(1003)
set.seed(1979)

# generate surfaces for hotspots in each month
rvr_dns_hot <- tibble(
  month,
  month_mu_x = c(-2, 0, -1, 1.5),
  month_mu_y = c(55, 58, 53.3, 56)
) |>
  expand_grid(hotspot_id = 1:3) |>
  mutate(
    mu_x = month_mu_x + runif(n(), -3, 3),
    mu_y = month_mu_y + runif(n(), -3, 3),
    dns_sigma = list(matrix(c(1,0.5,0.5,1), ncol=2))
  ) |>
  mutate(
    dns_pbdst = dist_multivariate_normal(list(c(mu_x, mu_y)), dns_sigma),
    .by = everything()
  ) |>
  mutate(
    dns = density(dns_pbdst, dns_srf_grd)
  )

# generate random samples of monthly density surfaces
rvr_dns <- rvr_dns_hot |>
  # aggregate over hotspots for monthly densities
  summarise(
    dns = list(purrr::reduce(dns, `+`)),
    .by = month
  ) |>
  expand_grid(
    iter = 1:n_samples
  ) |>
  # generate randomness, normalize and apply scale factor (i.e. total 10k animals)
  mutate(
    dns = purrr::map(dns, \(x){
      x <- x * runif(length(x), 0.95, 1.05)
      tibble(counts = x/sum(x) * 10000) |>
        bind_cols(dns_srf_grd)
    })
  ) |>
  unnest(dns) |>
  stars::st_as_stars(dims = c("x", "y", "month", "iter")) |>
  st_set_crs(4326)


# rvr_dns |>
#  filter(iter == 4) |>
#  plot(axes = TRUE, col = met.brewer("Johnson", 30, direction = -1), breaks = "equal")


# build <species> object ----------------------------------------------
rover <- Species(
  id = "rvr",
  role = "agent",
  common_name = "Rover",
  scientific_name = "Rover Vulgaris",
  body_mass_distr = VarDist(dist_normal(1000, 0.2 * 1000), units = "grams"),
  mortality_thresh_distr = VarDist(dist_uniform(300, 350), units = "grams"),
  spatial_distr = rvr_dns,
  behaviour_profile = rvr_behav,
  impact_responses = rvr_imp_resp
)


usethis::use_data(rover, overwrite = TRUE, compress = "xz")

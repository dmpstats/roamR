library(dplyr)
library(tidyr)
library(purrr)
library(distributional)
library(sf)
library(ggplot2)
library(stars)
library(MetBrewer)


mock_extent <- list(xmin = -4.5, ymin = 55.5, xmax = 3, ymax = 61)
mock_bbox <- structure(unlist(mock_extent), class = "bbox") |> st_set_crs(4326)

# -------------------------------------------------------------------------- #
#
#         Create a <Driver> objects for a mock scenario                   ----
#
# -------------------------------------------------------------------------- #

## "Land" driver  ----------------------------------------

uk_land <- ggplot2::map_data("world", region = "UK") |>
  st_as_sf(coords = c("long", "lat"),  crs = 4326) |>
  group_split(group) |>
  purrr::map(\(x){
    st_combine(x) |>
      st_cast("POLYGON")
  } ) |>
  purrr::list_c() |>
  st_combine()

ggplot() + geom_sf(data = uk_land)


drv_land <- Driver(
  id = "land",
  type = "habitat",
  descr = "land",
  sf_obj = uk_land,
  sf_descr = "UK Coastline",
  obj_active = "sf"
)


# Set as {raomR} data
usethis::use_data(uk_land, overwrite = TRUE, compress = "xz")
usethis::use_data(drv_land, overwrite = TRUE, compress = "xz")


## "Spatial Distribution" driver  -------------------------------------------------

# generate spatial grid
dns_srf_grd <- expand_grid(
  #x = seq(-3.5, 3, by = 0.1),
  #y = seq(55, 60, by = 0.1)
  x = seq(mock_extent$xmin - 0.1, mock_extent$xmax + 0.1, by = 0.1),
  y = seq(mock_extent$ymin - 0.1, mock_extent$ymax + 0.1, by = 0.1)
) |> as.matrix()

plot(dns_srf_grd)

# set months and nr of samples
month <- month.abb[1:4]
n_samples <- 5

#set.seed(1003)
set.seed(1979)

# generate surfaces for hotspots in each month
rvr_dns_hot <- tibble(
  month,
  month_mu_x = c(-2, 0, -1, 1.5), #, 1, 3),
  month_mu_y = c(54, 60, 53.3, 56)#, 50, 61)
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
  st_set_crs(4326) |>
  mutate(counts = units::set_units(counts, "count"))

rvr_dns |>
  filter(iter == 4) |>
  plot(axes = TRUE, col = met.brewer("Johnson", 30, direction = -1), breaks = "equal")


# construct driver
drv_sp_distr <- Driver(
  id = "rvr_distr",
  type = "habitat",
  descr = "Spatial distribution",
  stars_obj = rvr_dns,
  stars_descr = "Rovers monthly density maps",
  obj_active = "stars"
)


# Set as {raomR} data
usethis::use_data(drv_sp_distr, overwrite = TRUE, compress = "xz")




## "SST" driver  -------------------------------------------------

# sst monthly averages from 1981-2023
# source: https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html
sst_month <- stars::read_stars("C:/Users/Bruno/Dropbox/ORJIP/DisNBS/data/env rasters/sst.mon.mean.nc", proxy = TRUE) |>
  #st_set_crs(4326) |>
  st_set_crs('OGC:CRS84') |>
  st_warp(st_as_stars(st_bbox(), dx = 0.25)) |>
  # crop to mack bounding box
  st_crop(mock_bbox + c(-0.1, -0.1, 0.1, 0.1))


st_bbox(sst_month)
sst_month

# compute the mean for each "month-of-year"
sst <- sst_month |>
  aggregate(
    by = \(x){ month <- months(x) |> factor(levels = month.name); month},
    FUN = mean
  ) |>
  setNames("sst_mean_moy") |>
  # fetch data to local environment
  st_as_stars() |>
  # rename dimension
  st_set_dimensions("geometry", names = "months") |>
  #st_set_crs(4326) |>
  aperm(c(2, 3, 1)) |>
  mutate(
    sst_mean_moy = units::set_units(sst_mean_moy, "degree_Celsius")
  )

plot(sst, mfrow = c(2, 6), axes = TRUE)
plot(sst, axes = TRUE)


# st_crs(sst)$proj4string
# st_crs(rvr_dns)$proj4string
# st_crs(owf_foots)$proj4string
#
# st_crs(sst)$epsg
# st_crs(owf_foots)$epsg
#
# sf::st_crop(sst, owf_foots)

ggplot() +
  geom_sf(data = drv_land@sf_obj) +
  geom_sf(data = st_as_sfc(mock_bbox), fill = NA, col = "red") +
  geom_stars(data = sst |> filter(months == "December"), alpha = 0.7)


# construct driver
drv_sst <- Driver(
  id = "sst",
  type = "habitat",
  descr = "Sea Surface Temperature",
  stars_obj = sst,
  stars_descr = "Monthly average SST maps",
  obj_active = "stars"
)

# Set as {raomR} data
usethis::use_data(drv_sst, overwrite = TRUE, compress = "xz")



## "Prey" driver -----------------------------------------

# Generate mock prey monthly density surfaces
prey_months <- month.abb
n_months <- length(prey_months)
n_hotspots <- 2
n_peaks <- n_months * n_hotspots

set.seed(16654)

# peaks points
prey_peaks <- runif(
  2 * n_peaks,
  min = c(mock_extent$xmin, mock_extent$ymin),
  max = c(mock_extent$xmax, mock_extent$ymax)
) |>
  matrix(ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x", "y")))

# spatial resolution
sp_res <- 0.2

# grid points in x and y axes
x_grid <- seq(mock_extent$xmin - 0.1, mock_extent$xmax + 0.1, by = sp_res)
y_grid <- seq(mock_extent$ymin - 0.1, mock_extent$ymax + 0.1, by = sp_res)

# mesh points
mesh <- expand_grid(
  x = x_grid,
  y = y_grid
) |> as.matrix()

prey_cov <- matrix(c(0.5,0.1,0.1,0.5), ncol=2)

prey_dens_raw <- apply(prey_peaks,1, function(x){
  dist_multivariate_normal(list(x), list(prey_cov))
}) |>
  sapply(density, at = mesh, simplify = TRUE)

# coerce as multidimensional array
prey_dens <- array(
  unlist(prey_dens_raw),
  dim = c(length(y_grid), length(x_grid), n_months, n_hotspots)
) |>
  aperm(c(2, 1, 3, 4))

# sum get monthly dens by summing over hotspots
prey_dens <- apply(prey_dens, c(1, 2, 3), sum)

# normalize
prey_dens <- sweep(prey_dens, MARGIN = 3, STATS = apply(prey_dens, 3, sum), FUN = "/")

# monthly scaling
prey_dens <- sweep(prey_dens, MARGIN = 3, STATS = runif(n_months, 1E4, 4E4), FUN = "*")


yumyum_dens <- st_as_stars(prey_dens) |>
  st_set_dimensions(1, values = x_grid, names = "x") |>
  st_set_dimensions(2, values = y_grid, names = "y") |>
  st_set_dimensions(3, values = prey_months, names = "month") |>
  st_set_crs(4326) |>
  setNames("prey") |>
  mutate(prey = units::set_units(prey, "count"))

plot(yumyum_dens, axes = TRUE, mfrow = c(2, 6))


# construct driver
drv_prey <- Driver(
  id = "prey_distr",
  type = "habitat",
  descr = "Prey Density Surfaces",
  stars_obj = yumyum_dens,
  stars_descr = "Monthly density surfaces of Yummy Inmybellis",
  obj_active = "stars"
)


# Set as {raomR} data
usethis::use_data(drv_prey, overwrite = TRUE, compress = "xz")




## "SSS" driver -----------------------------------------

## Upload Sea Surface Salinity maps
# SOURCE:  https://data.ceda.ac.uk/download?path=/neodc/esacci/sea_surface_salinity/data/v04.41/GLOBALv4.41/30days/2021
# only year 2021

sss <- fs::dir_map("C:/Users/Bruno/Dropbox/ORJIP/DisNBS/data/env rasters/sss_esa_neodc//esacci/sea_surface_salinity/data/v04.41/GLOBALv4.41/30days/2021/", \(x) read_ncdf(x, var = "sss"))
sss <- do.call("c", sss)

plot(sss[mock_bbox])

sss_moy <- sss |>
  select(sss) |>
  st_crop(mock_bbox + c(-0.1, -0.1, 0.1, 0.1)) |>
  aggregate(
    by = \(x){ month <- months(x) |> factor(levels = month.name); month},
    FUN = mean
  ) |>
  setNames("sss_mean_moy") |>
  # rename dimension
  st_set_dimensions("geometry", names = "months") |>
  aperm(c(2, 3, 1)) |>
  mutate(
    sss_mean_moy = units::set_units(sss_mean_moy, "ppt")
  )

plot(sss_moy, axes = TRUE, mfrow = c(2, 6))


ggplot() +
  geom_sf(data = drv_land@sf_obj) +
  geom_sf(data = st_as_sfc(mock_bbox), fill = NA, col = "red") +
  geom_stars(data = sss_moy |> filter(months == "December"), alpha = 0.7)


# construct driver
drv_sss <- Driver(
  id = "sss",
  type = "habitat",
  descr = "Monthly Sea Surface Salinity",
  stars_obj = sss_moy,
  stars_descr = "Monthly average SSS",
  obj_active = "stars"
)


# # Set as {raomR} data
# usethis::use_data(drv_sss, overwrite = TRUE, compress = "xz")




## "OWFs" driver -----------------------------------------

# Generate random mock Offshore Wind Farms' footprints

# number of footprints
n_owfs <- 5

set.seed(399)
owf_foots <- st_as_sfc(mock_bbox) |>
  # get sea by extracting land
  st_difference(uk_land) |>
  # generate random owfs positions
  st_sample(n_owfs) |>
  st_buffer(20000) |>
  # generate footprints - i.e. polygons inside each position
  sapply(\(x){
    x |>
      st_sample(5) |>
      st_combine() |>
      st_convex_hull()
  }) |>
  st_sf(owf = letters[1:n_owfs], geometry = _) |>
  st_set_crs(st_crs(mock_bbox))


ggplot() +
  geom_sf(data = drv_land@sf_obj) +
  geom_sf(data = st_as_sfc(mock_bbox), col = "orange", fill = NA) +
  geom_sf(data = owf_foots, fill = "red", col = "firebrick", alpha = 0.2)



# # Generate mock Offshore Wind Farm footprint
# owf_foot <- st_polygon(
#   list(
#     rbind(
#       c(-1.2, 56.9),
#       c(-1.2, 57),
#       c(-1.05, 57.15),
#       c(-0.85, 57.15),
#       c(-1., 57),
#       c(-1., 56.9),
#       c(-1.2, 56.9)
#     )
#   )
# ) |>
#   st_sfc(crs = 4326)
#
# plot(owf_foot)
#
# ggplot() +
#   geom_stars(data = drv_sp_distr@stars_obj |> filter(month == "Feb", iter == 2)) +
#   geom_sf(data = drv_land@sf_obj) +
#   geom_sf(data = owf_foot, fill = "red", col = "firebrick", alpha = 0.2)



# construct driver
drv_owfs <- Driver(
  id = "owf_foot",
  type = "disturbance",
  descr = "OWF Footprints",
  sf_obj = owf_foots,
  obj_active = "sf"
)

usethis::use_data(owf_foots, overwrite = TRUE, compress = "xz")
usethis::use_data(drv_owfs, overwrite = TRUE, compress = "xz")







## "Fishing Ground" driver -----------------------------------------

fra_foot <- st_multipoint(
  matrix(c(
    -1.1, 56.7,
    -1.2, 56.6,
    -1, 56.7,
    -0.99, 56.6,
    -1.1, 56.55
  ), ncol = 2, byrow = TRUE)
) |>
  st_buffer(0.1) |>
  st_sfc(crs = 4326)

plot(fra_foot)

ggplot() +
  geom_stars(data = drv_sp_distr@stars_obj |> filter(month == "Feb", iter == 2)) +
  geom_sf(data = drv_land@sf_obj) +
  geom_sf(data = owf_foots, fill = "red", col = "firebrick", alpha = 0.2) +
  geom_sf(data = fra_foot, fill = "green", col = "darkgreen", alpha = 0.2)



# construct driver
drv_trawling <- Driver(
  id = "trawling_area",
  type = "disturbance",
  descr = "Fishing Restricted Area for Trawling",
  sf_obj = fra_foot,
  obj_active = "sf"
)



## Combine drivers into a list and write out to as package data -----------------------------------------

rover_drivers <- list(
  drv_land = drv_land,
  drv_owfs = drv_owfs,
  drv_sp_distr = drv_sp_distr,
  drv_sst = drv_sst,
  drv_prey = drv_prey,
  drv_trawling = drv_trawling,
  drv_sss = drv_sss
)

usethis::use_data(rover_drivers, overwrite = TRUE, compress = "xz")




# -------------------------------------------------------------------------- #
#
#  Create a <Species> object for a mock species ('Rover') to act as an agent ----
#
# -------------------------------------------------------------------------- #

## Species state profile  ----------------------------------

swim_cost_fn <- function(sst, int){
  x <- int - (2.75 * sst)
  max(x, 1, na.rm = TRUE) # na.rm also ensures minimum cost if agent lands in cell without SST surface coverage
}


water_rest_cost_fn <- function(b) sqrt(b)


mu_f <- 141
sigma_f <- 66
flight_cost_dist <- dist_lognormal(
  mu = log(mu_f/sqrt(mu_f^2 + sigma_f^2)),
  sigma = sqrt(log(1 + sigma_f^2/mu_f^2))
)

rvr_states <- list(
  flight = State(
    id = "flying",
    energy_cost = VarDist(flight_cost_dist, "kJ/hour"),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
    speed = VarDist(dist_uniform(10, 20), "m/s")
  ),
  dive = State(
    id = "foraging",
    energy_cost = VarDist(dist_uniform(3, 5), "kJ/hour"),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day")
  ),
  swimming = State(
    id = "swimming",
    energy_cost = VarFn(
      swim_cost_fn,
      list(sst = "driver", int = VarDist(dist_normal(113, 22))),
      units = "kJ/hour"
    ),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
    speed = VarDist(dist_uniform(0, 2), "m/s")
  ),
  water_rest = State(
    id = "water_resting",
    energy_cost = VarFn(
      water_rest_cost_fn,
      list("body_mass"),
      "kJ/hour"
    ),
    time_budget = VarDist(dist_uniform(1, 3), "hours/day")
  )
)


State(
  id = "flying",
  energy_cost = VarDist(flight_cost_dist, "kJ/hour"),
  time_budget = VarDist(dist_uniform(1, 3), "hours/day"),
  speed = VarFn(
    fn = \(min, max) runif(1, min, max),
    args_spec = list(min = 10, max = 20),
    units = "m/s"
  )
)



usethis::use_data(rvr_states, overwrite = TRUE, compress = "xz")


## Species driver responses  ----------------------------------

# exponential decay
exp_decay <- function(x, slope = 1/2){
  exp(-slope * x)
}


### land response
resp_land <- new(
  "DriverResponse",
  driver_id = "land",
  movement = MoveInfluence(
    prob = VarDist(distributional::dist_degenerate(1)),
    fn = exp_decay,
    type = "repulsion",
    mode = "vector-field",
    sim_stage = "bsln-imp"
  )
)

### spatial distr response
resp_spdist <- new(
  "DriverResponse",
  driver_id = "rvr_distr",
  movement = MoveInfluence(
    prob = VarDist(distributional::dist_degenerate(1)),
    fn = function(x) x,
    type = "attraction",
    mode = "cell-value",
    sim_stage = "bsln-imp"
  )
)

### SST response
resp_sst <- new(
  "DriverResponse",
  driver_id = "sst",
  movement = MoveInfluence(
    prob = VarDist(distributional::dist_degenerate(1)),
    fn = function(x) x,
    type = "attraction",
    mode = "cell-value",
    sim_stage = "bsln-imp"
  ),
  condition = "DEE"
)


### prey spatial distribution response
resp_sst <- new(
  "DriverResponse",
  driver_id = "prey_distr",
  movement = MoveInfluence(
    prob = VarDist(distributional::dist_degenerate(1)),
    fn = function(x) x,
    type = "attraction",
    mode = "cell-value",
    sim_stage = "bsln-imp"
  )
)



### OWF response
resp_owf <- new(
  "DriverResponse",
  driver_id = "owf_foot",
  movement = MoveInfluence(
    prob = VarDist(dist_beta(2, 8)),
    fn = exp_decay,
    type = "repulsion",
    mode = "vector-field",
    sim_stage = "imp"
  ),
  states = list(
    StateInfluence(
      state_id = "flying",
      prob = VarDist(dist_beta(1, 5)),
      extent = VarDist(dist_lognormal(2, 1), "hr/day")
    )
  )
)



### fishing ground response
resp_trawling <- new(
  "DriverResponse",
  driver_id = "trawling_area",
  movement = MoveInfluence(
    prob = VarDist(dist_beta(2, 8)),
    fn = exp_decay,
    type = "attraction",
    mode = "vector-field",
    sim_stage = "imp"
  ),
  states = list(
    StateInfluence(
      state_id = "foraging",
      prob = VarDist(dist_beta(1, 5)),
      extent = VarDist(dist_lognormal(2, 1), "hr/day")
    ),
    StateInfluence(
      state_id = "swimming",
      prob = VarDist(dist_beta(2, 3)),
      extent = VarDist(dist_lognormal(1, 1), "hr/day")
    )
  )
)



## Build <Species> object ----------------------------------------------
rover <- Species(
  id = "rover",
  common_name = "Rover",
  scientific_name = "Rover Vulgaris",
  body_mass_distr = VarDist(dist_normal(1000, 0.2 * 1000), units = "grams"),
  energy_to_mass_distr = VarDist(0.072, "g/kJ"),
  mortality_thresh_distr = VarDist(dist_uniform(300, 350), units = "grams"),
  states_profile = rvr_states,
  driver_responses = list(
    resp_land,
    resp_spdist,
    resp_sst,
    resp_owf,
    resp_trawling
  )
)

## Set as {raomR} data ---------------------------------------
usethis::use_data(rover, overwrite = TRUE, compress = "xz")





# -------------------------------------------------- #
#
#             Create a <IBM> object               ----
#
# -------------------------------------------------- #

## Build <ModelConfig> object ----------------------------------------------

### starting sites
coast_pts <- st_buffer(uk_land, 50) |>
  st_crop(st_as_sfc(mock_bbox)) |>
  st_cast("MULTILINESTRING") |>
  st_cast("LINESTRING") |>
  st_cast("POINT")

plot(coast_pts)


set.seed(1015)
set.seed(1019)
sites3 <- coast_pts[sample(length(coast_pts), 3),]

st_crs(sites3) <- st_crs(coast_pts)

sites3 <- st_as_sf(sites3) |>
  mutate(
    id = c("A", "B", "C"),
    prop = c(0.30, 0.30, 0.40)
  ) |>
  st_set_geometry("geom")

plot(coast_pts)
plot(sites3["id"], add = TRUE, col = "red", pch = 19)

ggplot() +
  geom_stars(data = drv_sp_distr@stars_obj |> filter(month == "Feb", iter == 2)) +
  geom_sf(data = drv_land@sf_obj) +
  geom_sf(data = sites3, col = "red") +
  geom_sf(data = st_as_sfc(mock_bbox), col = "orange", fill = NA)


ibm_config_rover <- ModelConfig(
  n_agents = 100,
  ref_sys = st_crs(4326),
  aoc_bbx = mock_bbox,
  delta_x = 0.1,
  delta_y = 0.1,
  delta_time = "1 day",
  start_date = as.Date("2022-09-01"),
  end_date = as.Date("2023-04-30"),
  start_sites = sites3
)


## Set as {raomR} data ---------------------------------------
usethis::use_data(ibm_config_rover, overwrite = TRUE, compress = "xz")


# Initialize IBM: build <IBM> object ----------------------------------------------
rover_ibm <- rmr_initiate(ibm_config_rover, rover, rover_drivers)

## Set as {raomR} data
usethis::use_data(rover_ibm, overwrite = TRUE, compress = "xz")










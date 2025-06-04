library(dplyr)
library(tidyr)
library(purrr)
library(distributional)
library(sf)
library(ggplot2)
library(stars)
library(MetBrewer)



mock_extent <- list(xmin = -4, xmax = 5, ymin = 52, ymax = 60)
mock_bbox <- structure(unlist(mock_extent), class = "bbox") |> st_set_crs(4326)


# -------------------------------------------------------------------------- #
#
#  Create a <Species> object for a mock species ('Rover') to act as an agent ----
#
# -------------------------------------------------------------------------- #

## Build the species behaviour profile  ----------------------------------
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

## Build the list of impact responses  -------------------------------------

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


## Create density surface  -------------------------------------------------

# generate spatial grid
dns_srf_grd <- expand_grid(
  x = seq(-3.5, 3, by = 0.1),
  y = seq(55, 60, by = 0.1)
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
  month_mu_y = c(54, 58, 53.3, 56)
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


rvr_dns |>
  filter(iter == 4) |>
  plot(axes = TRUE, col = met.brewer("Johnson", 30, direction = -1), breaks = "equal")



## Build <Species> object ----------------------------------------------
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

## Set as {raomR} data ---------------------------------------
usethis::use_data(rover, overwrite = TRUE, compress = "xz")





# -------------------------------------------------------------------------- #
#
#         Create a <Habitat> object for a mock scenario                   ----
#
# -------------------------------------------------------------------------- #


## Land polygon for the slot "terrain" ----------------------------------------
uk_land <- ggplot2::map_data("world", region = "UK") |>
  st_as_sf(coords = c("long", "lat"),  crs = 4326) |>
  group_split(group) |>
  purrr::map(\(x){
    st_combine(x) |>
      st_cast("POLYGON")
  } ) |>
  purrr::list_c() |>
  st_combine()


ggplot() +
  geom_stars(data = rover@spatial_distr |> filter(month == "Apr", iter == 1)) +
  geom_sf(data = uk_land)



## Get land polygon for @terrain -------------------------------------------------

### download bathymetry from ETOPO 2022 database on NOAA website via {marmap}
bathy <- marmap::getNOAA.bathy(mock_extent$xmin, mock_extent$xmax, mock_extent$ymin, mock_extent$ymax) |>
  marmap::as.raster() |>
  stars::st_as_stars()

plot(bathy, axes = TRUE)
st_bbox(bathy)


## Get SST for @sst -------------------------------------------------

# sst monthly means from 1981-2023
# source: https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html
sst_month <- stars::read_stars("data-raw/sst.mon.mean.nc", proxy = TRUE) |>
  st_set_crs(4326) |>
  # crop to mack bounding box
  st_crop(mock_bbox)

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



## Generate mock prey monthly density surfaces -----------------------------------------

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
x_grid <- seq(mock_extent$xmin, mock_extent$xmax, by = sp_res)
y_grid <- seq(mock_extent$ymin, mock_extent$ymax, by = sp_res)

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
  setNames("counts")


plot(yumyum_dens, axes = TRUE, mfrow = c(2, 6))


## Set prey density surfaces in <Species object ---------------------------------
yumyum <- Species(
  id = "yum",
  role = "prey",
  common_name = "yumyum",
  scientific_name = "Yummy Inmybellis",
  spatial_distr = yumyum_dens
)



## Upload Sea Surface Salinity maps -----------------------------------------
# SOURCE:  https://data.ceda.ac.uk/download?path=/neodc/esacci/sea_surface_salinity/data/v04.41/GLOBALv4.41/30days/2021
# only year 2021

sss <- fs::dir_map("data-raw/sss_esa_neodc//esacci/sea_surface_salinity/data/v04.41/GLOBALv4.41/30days/2021/", \(x) read_ncdf(x, var = "sss"))
sss <- do.call("c", sss)

plot(sss[mock_bbox])

sss_moy <- sss |>
  select(sss) |>
  st_crop(mock_bbox) |>
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



## Upload wind maps  -----------------------------------------

# https://data.marine.copernicus.eu/product/WIND_GLO_PHY_CLIMATE_L4_MY_012_003/services

fs::dir_ls("data-raw/wind_copernicus_2021-2023/")

wind <- fs::dir_map(
  "data-raw/wind_copernicus_2021-2023/",
  fun = \(x) read_ncdf(x, var = c("wind_speed", "northward_wind", "eastward_wind"))
) |>
  map(~st_crop(.x, mock_bbox))

wind <- do.call("c", wind) |>
  mutate(
    # Unsure if it makes sense to average wind direction over these temporal scales.
    # So, adding it for the moment for testing purposes
    # Based on https://confluence.ecmwf.int/pages/viewpage.action?pageId=133262398
    wind_direction = units::drop_units(atan2(northward_wind, eastward_wind)) * 180/pi + 180 %% 360,
    wind_direction = units::set_units(wind_direction, "degrees")
  ) |>
  # get the averages per month-of-years
  aggregate(
    by = \(x){ month <- months(x) |> factor(levels = month.name); month},
    FUN = mean
  ) |>
  # rename dimension
  st_set_dimensions("geometry", names = "months") |>
  aperm(c(2, 3, 1))


plot(wind["wind_speed"], axes = TRUE, mfrow = c(2, 6))
plot(wind["wind_direction"], axes = TRUE, mfrow = c(2, 6))


## Build <Habitat> object ----------------------------------------------
atlantis <- Habitat(
  terrain = uk_land,
  bathymetry = bathy,
  prey = yumyum,
  sst = sst,
  sss = sss_moy,
  wind = wind)


## Set as {raomR} data ---------------------------------------
usethis::use_data(atlantis, overwrite = TRUE, compress = "xz")






# -------------------------------------------------------------------------- #
#
#         Create a list of <Structure> object for a mock scenario         ----
#
# -------------------------------------------------------------------------- #


## Generate mock Offshore Wind Farm footprint -----------------------------------------

owf_foot <- st_polygon(
  list(
    rbind(
      c(-1.2, 56.9),
      c(-1.2, 57),
      c(-1.05, 57.15),
      c(-0.85, 57.15),
      c(-1., 57),
      c(-1., 56.9),
      c(-1.2, 56.9)
    )
  )
) |>
  st_sfc(crs = 4326)

plot(owf_foot)

ggplot() +
  geom_stars(data = rover@spatial_distr |> filter(month == "Feb", iter == 2)) +
  geom_sf(data = atlantis@terrain) +
  geom_sf(data = owf_foot, fill = "red", col = "firebrick", alpha = 0.2)



## Generate mock shipping corridor footprint -----------------------------------------

traffic_foot <- st_polygon(
  list(
    rbind(
      c(-2.15, 57.),
      c(-1.15, 57.),
      c(-1.15, 57.05),
      c(-2.15, 57.05),
      c(-2.15, 57.)
    )
  )
) |>
  st_sfc(crs = 4326)

ggplot() +
  geom_stars(data = rover@spatial_distr |> filter(month == "Feb", iter == 2)) +
  geom_sf(data = atlantis@terrain) +
  geom_sf(data = traffic_foot, fill = "yellow", col = "orange", alpha = 0.2) +
  geom_sf(data = owf_foot, fill = "red", col = "firebrick", alpha = 0.2)



## Generate mock fishing ground footprint -----------------------------------------

fishing_foot <- st_multipoint(
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

plot(fishing_foot)

ggplot() +
  geom_stars(data = rover@spatial_distr |> filter(month == "Feb", iter == 2)) +
  geom_sf(data = atlantis@terrain) +
  geom_sf(data = traffic_foot, fill = "yellow", col = "orange", alpha = 0.2) +
  geom_sf(data = owf_foot, fill = "red", col = "firebrick", alpha = 0.2) +
  geom_sf(data = fishing_foot, fill = "green", col = "darkgreen", alpha = 0.2)



## Build list of <Structure> objects ----------------------------------------------

list(
  owf = Structure(id = "owf", type = "owf", boundary = owf_foot, height = units::set_units(100, "m")),
  shipping_corridor = Structure(id = "ships", type = "traffic", boundary = traffic_foot),
  fishing_ground = Structure(id = "fishing", type = "fishing", boundary = fishing_foot)
)







ModelConfig(
  n_agents = 1000,
  ref_sys = st_crs(4326),
  aoc_bbx = c(0, 57, 2,58),
  delta_x = 0.1,
  delta_y = 0.1,
  delta_time = "1 day",
  start_date = "2022-09-01",
  start_date = "2023-04-31"
)



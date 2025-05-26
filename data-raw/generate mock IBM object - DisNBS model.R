

# helper to clip <stars> datacubes based on polys
clip_holes <- function(x, y){

  holes <- sf::st_crop(x, y, as_points = FALSE)

  holes_pts <- holes |>
    sf::st_as_sf(as_points = TRUE) |>
    sf::st_geometry()

  holes_cells_idx <- stars::st_cells(x, holes_pts)

  ndims <- length(dim(x))

  x_holed <- if(ndims > 2){
    stars::st_apply(x, 3:ndims, function(.x){
      .x[holes_cells_idx] <- NA
      .x
    })
    }else{
      x[holes_cells_idx] <- NA
    }

#   x_holed <- stars::st_apply(x, 3, function(.x){
#     .x[holes_cells_idx] <- NA
#     .x
#   })

  x_holed
}

# Drivers ------------------------------------------------------------

mock_extent <- list(xmin = -4.5, ymin = 55.5, xmax = 3, ymax = 61)
mock_bbox <- structure(unlist(mock_extent), class = "bbox") |> st_set_crs(4326)


## Monthly density surfaces ----

# generate spatial grid
srf_grd <- tidyr::expand_grid(
  #x = seq(-3.5, 3, by = 0.1),
  #y = seq(55, 60, by = 0.1)
  x = seq(mock_extent$xmin - 0.1, mock_extent$xmax + 0.1, by = 0.1),
  y = seq(mock_extent$ymin - 0.1, mock_extent$ymax + 0.1, by = 0.1)
) |> as.matrix()

plot(srf_grd)

# set months and nr of samples
months <- month.abb[c(1:3, 10:12)]
n_samples <- 5

#set.seed(1003)
set.seed(1979)

# generate surfaces for hotspots in each month
dns_hot <- tidyr::tibble(
  months,
  month_mu_x = c(-2, 0, -1, 1.5, 1, 2),
  month_mu_y = c(54, 60, 53.3, 56, 52, 59)
) |>
  tidyr::expand_grid(hotspot_id = 1:3) |>
  dplyr::mutate(
    mu_x = month_mu_x + runif(dplyr::n(), -3, 3),
    mu_y = month_mu_y + runif(dplyr::n(), -3, 3),
    dns_sigma = list(matrix(c(1,0.5,0.5,1), ncol=2))
  ) |>
  dplyr::mutate(
    dns_pbdst = distributional::dist_multivariate_normal(list(c(mu_x, mu_y)), dns_sigma),
    .by = dplyr::everything()
  ) |>
  dplyr::mutate(
    dns = density(dns_pbdst, srf_grd)
  )

# generate random samples of monthly density surfaces
dns <- dns_hot |>
  # aggregate over hotspots for monthly densities
  dplyr::summarise(
    dns = list(purrr::reduce(dns, `+`)),
    .by = months
  ) |>
  tidyr::expand_grid(
    iter = 1:n_samples
  ) |>
  # generate randomness, normalize and apply scale factor (i.e. total 10k animals)
  dplyr::mutate(
    dns = purrr::map(dns, \(x){
      x <- x * runif(length(x), 0.95, 1.05)
      dplyr::tibble(counts = x/sum(x) * 10000) |>
        dplyr::bind_cols(srf_grd)
    })
  ) |>
  tidyr::unnest(dns) |>
  stars::st_as_stars(dims = c("x", "y", "months", "iter")) |>
  sf::st_set_crs(4326) |>
  dplyr::mutate(counts = units::set_units(counts, "count"))

dns |>
  dplyr::filter(iter == 4) |>
  plot(axes = TRUE, col = MetBrewer::met.brewer("Johnson", 30, direction = -1), breaks = "equal")

drv_dens <- Driver(
  id = "dens",
  type = "resource",
  descr = "Density",
  stars_obj = dns,
  stars_descr = "Monthly density maps",
  obj_active = "stars"
)




## impacted density surfaces ------
# For testing purposes, apply truncation-type impact, i.e. densities in clipped
# areas are not redistributed
imp_dens <- clip_holes(dns, owf_foots |> sf::st_buffer(20000) ) |>
  dplyr::mutate(counts = units::set_units(counts, "counts"))

plot(imp_dens)
plot(imp_dens[, , , 1, , drop = TRUE])


drv_imp_dens <- Driver(
  id = "imp_dens",
  type = "resource",
  descr = "Impacted Density",
  stars_obj = imp_dens,
  stars_descr = "Monthly impacted density maps",
  obj_active = "stars"
)


## Monthly energy intake surfaces ----------------------------------
energy_intake <- stars_obj(drv_prey) |>
  dplyr::filter(month %in% months) |>
  setNames("intake") |>
  dplyr::mutate(
    intake = units::drop_units(intake),
    intake = units::set_units(intake, "kJ/hr")
  )

plot(energy_intake)


drv_intake <- Driver(
  id = "intake",
  type = "resource",
  descr = "Energy Intake",
  stars_obj = energy_intake,
  stars_descr = "Monthly energy intake maps",
  obj_active = "stars"
)



## Monthly impacted energy intake surfaces ------
# impact applied randomly, i.e. not spatially explicit nor relative to location of impacts
imp_energy_intake <- stars::st_apply(energy_intake, c(1, 2, 3), \(x) x * runif(1, 0.4, 0.8)) |>
  dplyr::mutate(intake = units::set_units(intake, "kJ/hr"))

plot(imp_energy_intake)

drv_imp_intake <- Driver(
  id = "imp_intake",
  type = "resource",
  descr = "Impacted Energy Intake",
  stars_obj = imp_energy_intake,
  stars_descr = "Monthly impacted energy intake maps",
  obj_active = "stars"
)


# monthly SST maps
drv_sst


# Species -----------------------------------------------------------------

resp_dens <- DriverResponse(
  driver_id = "dens",
  movement = MoveInfluence(
    prob = VarDist(distributional::dist_degenerate(1)),
    type = "attraction",
    mode = "cell-value",
    sim_stage = "bsln"
  )
)

resp_imp_dens <- DriverResponse(
  driver_id = "imp_dens",
  movement = MoveInfluence(
    prob = VarDist(distributional::dist_normal(0.67, sd = 0.061)),
    type = "attraction",
    mode = "cell-value",
    sim_stage = "imp"
  )
)


spp <- Species(
  id = "rover",
  common_name = "Rover",
  scientific_name = "Rover Vulgaris",
  body_mass_distr = VarDist(distributional::dist_normal(1000, 0.2 * 1000), units = "grams"),
  energy_to_mass_distr = VarDist(0.072, "g/kJ"),
  mortality_thresh_distr = VarDist(distributional::dist_uniform(300, 350), units = "grams"),
  states_profile = rvr_states,
  driver_responses = list(
    resp_dens,
    resp_imp_dens
  )
)


rvr_states$dive@id



# IBM object -----------------------------------------------------------------
cfg <- ibm_config_rover
cfg@start_date <- as.Date("2022-10-01")
cfg@end_date <- as.Date("2023-03-30")

rover_ibm_disnbs <- rmr_initiate(
  model_config = cfg,
  species = spp,
  drivers = list(
    drv_dens,
    drv_intake,
    drv_imp_dens,
    drv_imp_intake,
    drv_sst
  )
)

usethis::use_data(rover_ibm_disnbs, overwrite = TRUE, compress = "xz")






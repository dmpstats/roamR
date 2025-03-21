library(tidyverse)
library(devtools)
library(sf)
library(stars)
load_all()

# UTM zone 30N
utm30 <- st_crs(32630)

source("movement_devel/sample_cell.R")
source("movement_devel/calc_day_costs.R")
source("movement_devel/sample_cell.R")

guill_imb_config <- readRDS("movement_devel/guill_ibm_config.rds")
guill <- readRDS("movement_devel/guill_species.rds")


density_map <- readRDS("vignettes/articles/data/GuillemotIsle of May3_iteration1.rds") %>%
  st_as_stars() %>%
  rename(pop_dens = X3)

dens_crop <- st_bbox(c(xmin = -5e5, ymin = -4e6,  xmax = 5e5, ymax = -3.2e6), crs = st_crs(density_map))

density_map <- st_crop(density_map, dens_crop)

template_rast <- st_as_stars(guill_imb_config@aoc_bbx, nx = dim(density_map)[1], ny = dim(density_map)[2], values = NA_real_)

density_map <- st_warp(density_map, template_rast)

footprints <- st_read("vignettes/articles/data/Synthetic Polygons/") %>%
  st_as_stars() %>%
  st_transform(utm30)

coastline <- ggplot2::map_data("world", region = "UK") |>
  st_as_sf(coords = c("long", "lat"),  crs = 4326) |>
  group_split(group) |>
  purrr::map(\(x){
    st_combine(x) |>
      st_cast("POLYGON")
  } ) |>
  purrr::list_c() |>
  st_combine() %>%
  st_transform(utm30) %>%
  st_crop(density_map)



work_agent <- Agent(species = guill, model_config = guill_imb_config)

work_agent@history$energy_expenditure <- work_agent@history$energy_expenditure %>%
  units::drop_units() %>%
  units::set_units(., "kJ")

work_agent@history <- work_agent@history %>%
  st_set_crs(guill_imb_config@ref_sys)


# Input data layers -----------------------------------------------------------------------------------------------

set.seed(4958)


system.time({

  current_time <- guill_imb_config@start_date + days(1)
  step_duration <- days(1)
  return_date <- current_time + days(270)

  # daylight_refs <- suncalc::getSunlightTimes(lat = work_agent@history$geometry[1], lon = ref_loc[2], date = as_date(current_time))

  while(current_time <= guill_imb_config@end_date){

    destination <- sample_cell(density_map, 1)

    new_time <- current_time + step_duration

    if(date(new_time) != date(current_time)) {

      energy_profile <- calc_day_cost(in_agent = work_agent, in_species = guill,
                            in_ibm = guill_ibm, sst = 8, intake = units::set_units(589.5, "kJ/h"))

      # existing activity profile - store
      energy_expenditure <- sum((energy_profile$prop*24) * energy_profile$unit_cost) %>%
        units::drop_units() %>%
        units::set_units(., "kJ")

      wt_gain <- units::drop_units(energy_expenditure) * 0.072 %>%
        units::set_units(., "g")

      # update activity profile for use in t+1
      nudge_states <- state_balance(in_states = energy_profile[1:4,], night_proportion = 0.3,
                                    energy_target = units::set_units(1.14, "kJ/h"))

      work_agent@condition@states_budget[1:4] <- nudge_states %>%
        units::set_units(., 1) %>%
        as.list()


    }

    if(month(new_time) != month(current_time)) {

      destination <- sample_cell(density_map, 1)

    }

    current_time <- new_time
    work_agent@condition@location[1:2] <- destination[1:2]
    work_agent@condition@body_mass <- work_agent@condition@body_mass + wt_gain

    agent_update <- sf::st_sf(timestep = work_agent@condition@timestep + 1,
                              body_mass = work_agent@condition@body_mass,
                              states_budget = list(work_agent@condition@states_budget),
                              energy_expenditure = energy_expenditure,
                              geometry = sf::st_sfc(work_agent@condition@location, crs = sf::st_crs(work_agent@history$geometry))
    )

    work_agent@history <- work_agent@history %>% bind_rows(agent_update)

  }
})


#plot(work_agent@history$geometry, pch = 19, cex = 0.5, col = "red")
plot(work_agent@history$body_mass)

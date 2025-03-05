# UTM zone 30N
utm30 <- st_crs(32630)

source("movement_devel/sample_cell.R")
guill_imb_config <- readRDS("movement_devel/guill_ibm_config.rds")
guill <- readRDS("movement_devel/guill_species.rds")


simBird <- Agent(species = guill, model_config = guill_imb_config)



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



simBird@history <- simBird@history %>%
  st_set_crs(guill_imb_config@ref_sys)


# step dest -------------------------------------------------------------------------------------------------------

dest_fn <- function(in_pt, in_dist, in_line){

  extent_poly <- st_buffer(x = in_pt, dist=in_dist)

  st_intersection(in_line, extent_poly)

}


# Input data layers -----------------------------------------------------------------------------------------------

set.seed(4958)

destination <- sample_cell(density_map, 1) %>%
  as.data.frame() %>%
  terra::vect(., geom = c("x", "y"), crs = "epsg:32630")


start <- simBird@properties@start_point %>%
  terra::vect()

crs(start) <- "epsg:32630"

path_calc <- spaths::shortest_paths(rast(density_map), start, destination, output = "lines") %>%
  st_as_sf()

day_dist <- units::drop_units(simBird@properties@speeds$flight*0.06*24*3600)/2

#day_path <- dest_fn(st_as_sf(start), day_dist, st_as_sf(path_calc))

n_pts <- st_length(path_calc)/day_dist

test_pts <- st_line_sample(test, sample = runif(as.integer(n_pts), 0, 1)) %>%
  st_jitter(1e4)


ggplot() +
  geom_stars(data = density_map, aes(fill = pop_dens)) +
  geom_sf(data = st_as_sf(destination), color = "red", fill = "red", alpha = 0.4) +
  geom_sf(data = st_as_sf(start), color = "green") +
  geom_sf(data = path_calc) +
  geom_sf(data =  test_pts, col = "white") +
  # geom_sf(data = day_path, color = "purple", linewidth = 2) +
  geom_sf(data = coastline) +
  geom_stars(data = footprints, fill = "lightblue", alpha = 0.4)




dest_ind <- sample(1:length(dest_vect), 1000, prob = dest_vect)

sampleLocs <- data.frame(x = dest_ind %% nrow(density_map),
                        y = ceiling(dest_ind/nrow(density_map)))

plot(density_map$pop_dens[sampleLocs$x, sampleLocs$y])


current_speed <- unlist(simBird@properties@speeds[current_state])

next_pos <- move_agent(current_pos, travel_dir, var_dir, current_speed)

current_dir <- calc_direction(current_pos, destination)

#dir_modifiers <- quick_extract_pt(test, next_pos)

dir_modifiers <- st_extract(guill_ibm@drivers$owf@stars_obj, at = current_pos)

# dir_modifiers <- quick_extract_pt(repel_array, next_pos)

# current_state <- sample(1:5, 1, prob = trans_mat[current_state, ])

# current_expenditure <- ind_mat[current_state, ] %*% state_expenditure

pars <- c(1, dir_modifiers$slope)
dirs <- c(current_dir, dir_modifiers$aspect)

y_tan <- sin(dirs) %*% pars
x_tan <- cos(dirs) %*% pars

travel_dir <- atan2(y_tan, x_tan)

current_pos <- next_pos

?nearest

densi

library(readr)
library(stars)
sst <- read_rds("vignettes/articles/data/sst_1_month.rds")
sst

# Best shot ATM seems to be st_warp() for x-axis conversion, followed by
# st_crop() for the AOC.

# Probably overcareful, but geting the correct names and positions of x/y dims in the
# data-cube from raster-level dimensions
sst_dims <- st_dimensions(sst)
xy_names <- attr(sst_dims, "raster")$dimensions
xy_pos <- which(attr(sst_dims, "names") %in% xy_names)
# get delta of x-axis
x_delta <- attributes(sst)$dimensions[[xy_pos[1]]]$delta
y_delta <- attributes(sst)$dimensions[[xy_pos[2]]]$delta

# warp 0:360 to -180:180 (based on examples in https://r-spatial.github.io/stars/reference/st_warp.html)
target_stars <- st_as_stars(st_bbox(), dx = x_delta, dy = y_delta)
# would need to be more careful for the generalized case, by calculating extent
# of the target stars based on the bbox of the original sst object, via a lon
# conversion [e.g. ((lon - 180) %% 360) - 180]. However, given this is handling
# provided data, which covers the whole word, using `st_bbox()` is appropriate

# quick check before overwriting
plot(sst, axes = TRUE) # weird automatic x-axis W/E plotting scale
plot(st_set_crs(sst, NA), axes = TRUE) # but looks OK when projection is removed and plot based solely on grid values :/
# anyway, st_warp seems to to the job
st_warp(sst, target_stars) |> plot(axes = TRUE)

# sst converted to sensible W/E coordinates
sst <- st_warp(sst, target_stars)


# crop to AOC
mock_aoc_bbox <- structure(c(xmin = 0, ymin = 50, xmax = 20, ymax = 60), class = "bbox") |> st_set_crs(4326)
sst_crop <- st_crop(sst, mock_aoc_bbox)
plot(sst_crop)


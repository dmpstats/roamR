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


# sst converted to sensible W/E coordinates
sst <- st_warp(sst, target_stars)



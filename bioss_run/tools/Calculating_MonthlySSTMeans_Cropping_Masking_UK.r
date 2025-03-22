setwd("C:/Users/stats/dmpstats.co.uk/DisNBS PSG - WP3/data/sst_file")
library(ncdf4)
library(raster)
library(terra)
library(sp)
library(oceanmap)

#read in the SST monthly means file
nc <- nc2raster("sst.mon.mean.nc","sst")

#Subset the file based on Years from 2018 to 2022
RasterStudyYears <- raster::stack(
raster::subset(nc, grep("X2018", names(nc), value=T)),
raster::subset(nc, grep("X2019", names(nc), value=T)),
raster::subset(nc, grep("X2020", names(nc), value=T)),
raster::subset(nc, grep("X2021", names(nc), value=T)),
raster::subset(nc, grep("X2022", names(nc), value=T))
)

#Extract the months, needed to average over
ExtractMonths = substr(names(RasterStudyYears),6,7)


#Call the command to calculate monthly means over the 5 years
RasterBrickperMonth = stackApply(RasterStudyYears, ExtractMonths, mean)
plot(RasterBrickperMonth)

#rename the months
names(RasterBrickperMonth) <- month.name

#specify UK coordinates
e <- as(extent(c(xmin= -10, xmax= 10, ymin= 50, ymax= 60)), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"

#crop the area based on coordinates
crop_UK <- crop(RasterBrickperMonth , e)
plot(crop_UK, axes=T)

#Mask the cropped area
mask_UK = mask(crop_UK,e)
plot(mask_UK, axes=T)



bioss_sst <- st_as_stars(mask_UK)
names(bioss_sst) <- "sst"

date_values <- date("2025-01-01") + months(0:11) + years(c(rep(1, 6), rep(0, 6)))

bioss_sst <- st_set_dimensions(bioss_sst, 3, values = date_values, names = "time")


bioss_sst <- st_transform(bioss_sst, st_crs(32630))

saveRDS(bioss_sst, "bioss_sst_stars.rds")





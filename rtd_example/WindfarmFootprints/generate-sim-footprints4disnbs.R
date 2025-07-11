## ###################################################
## Code to simulate wind farm polygons to use in the ORJIP DisNBS project
## Created AB 3 Feb 2025

## ###################################################
## Load libraries

library(sf)
library(terra)

# ###################################################
## Set paths

mypath <- file.path("C:", "Users", "adam", "Desktop", "ACTIVE", "DisNBS", "WindfarmFootprints")

source(file.path(mypath, "functions-sim-footprints4disnbs.R"))

## ###################################################
## Read in Buckingham et al. data

selfoot <- sf::read_sf(file.path(mypath, "Polygons From Buckingham Et Al", "North_Sea_OWF_2030_polygons"))

## ###################################################
## Set seed to use (set to ensure reproducibility)

set.seed(5465) 

## ###################################################
## options to use for perturbing actual footprints (for each footprint 
##  a random option is selected for each of these)

simfoot <- make_perturbed_polygons(selfoot, dshift_range = c(2, 5), sca_range = c(10, 50))
  
## ###################################################
## Plot

## plot(selfoot[,2], xlim=st_bbox(allfoot)[c(1,3)], ylim=st_bbox(allfoot)[c(2,4)], col="green")

## ###################################################
## Plot to see overlap betrween grid and footprints

tmpl.land <- terra::rast(file.path(mypath, "atseagrid.tif"))

tiff(filename = file.path(mypath, "Synthetic Polygons", "Synthetic_North_Sea_OWF_2030_polygons.tif"),
     compression = "none", width=950, height=820)

par(mfrow=c(1,2), mar=c(3,3,0,0))

plot(terra::project(tmpl.land, "EPSG:4326"), col="blue", legend=FALSE, main="Published", xlim=c(-5, 9), ylim=c(50.5, 61.5),
     cex.axis=1.7, cex.main=1.7, cex.lab=1.7)
plot(sf::st_transform(selfoot, crs="EPSG:4326"), add=TRUE, col="orange")

plot(terra::project(tmpl.land, "EPSG:4326"), col="blue", legend=FALSE, main="Synthetic", xlim=c(-5, 9), ylim=c(50.5, 61.5),
     cex.axis=1.7, cex.main=1.7, cex.lab=1.7)
plot(sf::st_transform(simfoot, crs="EPSG:4326"), add=TRUE, col="orange")

dev.off()

## ###################################################

sf::write_sf(simfoot, file.path(mypath, "Synthetic Polygons", "Synthetic_North_Sea_OWF_2030_polygons.shp"))
 
## ###################################################
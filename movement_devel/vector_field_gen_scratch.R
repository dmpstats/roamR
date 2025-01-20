library(terra)
library(sf)
library(tidyverse)
library(ggquiver)


mayRast <- readRDS("../../../data/data/guillemot isle of may monthly raster sims/GuillemotIsle of May1_iteration1.rds")
plot(mayRast)

e <- ext(-1e6, 1e6, -4e6, -3e6)

test <- rast(mayRast)

cropRast <- crop(test, e)

plot(cropRast)

values(cropRast) <- ifelse(is.na(values(cropRast)), -1, NA)

distRast <- distance(cropRast)
plot(distRast)

rasterVis::vectorplot(distRast, reverse = T, col = "white") 




raster2quiver <- function(rast, aggregate = 50, colours = terrain.colors(6))
{
  names(rast) <- "z"
  quiv <- aggregate(rast, aggregate)
  terr <- terrain(quiv, v = c('slope', 'aspect'))
  quiv$u <- terr$slope[] * sin(terr$aspect[])
  quiv$v <- terr$slope[] * cos(terr$aspect[])
  quiv_df <- as.data.frame(quiv, xy = TRUE)
  rast_df <- as.data.frame(rast, xy = TRUE)
  
 # print(
    ggplot(mapping = aes(x = x, y = y, fill = z)) + 
          geom_raster(data = rast_df, na.rm = TRUE) + 
          geom_quiver(data = quiv_df, aes(u = u, v = v), col = "white", vecsize = 1.5) +
          scale_fill_gradientn(colours = colours, na.value = "transparent") +
          theme_bw()
    #)
  
 # return(quiv_df)
}

vPlot <- raster2quiver(distRast, aggregate = 2, colours = viridisLite::viridis(100)) +
  ggtitle("Example vector fields", "Large scale influences") +
  xlab("Easting") + ylab("Northing") +
  coord_equal()

ggsave("docs/images/vectorplot.png", vPlot, width = 20, height = 20, dpi = "retina")


vPlot_v <- raster2quiver(distRast, aggregate = 2, colours = viridisLite::viridis(100)) +
  xlab("Easting") + ylab("Northing") 


vPlot_p <- raster2quiver(distRast, aggregate = 2, colours = viridisLite::plasma(100)) +
  xlab("Easting") + ylab("Northing") 


ggsave("docs/images/vectorplot_v.png", vPlot_v, width = 10, height = 10, dpi = 300)
ggsave("docs/images/vectorplot_p.png", vPlot_p, width = 10, height = 10, dpi = 300)


grad <-ctmcmove::rast.grad(distRast) 
grad.x <-grad$rast.grad.x 
grad.y <-grad$rast.grad.y









plot(grad.x, col = viridisLite::viridis(100))

gradMag <- grad.x
values(gradMag) <- sqrt(values(grad.x)^2 + values(grad.y)^2)
plot(gradMag, col = viridisLite::viridis(100))

gradDir <- grad.x
values(gradDir) <- atan2(values(grad.x), values(grad.y))
plot(gradDir, col = viridisLite::viridis(100))

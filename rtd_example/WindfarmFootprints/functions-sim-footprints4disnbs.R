## ###################################################
## Code to simulate wind farm polygons to use in the ORJIP DisNBS project
## Created AB 3 Feb 2025, updated 5 Feb 2025

make_perturbed_polygons <- function(selfoot, dshift_range, sca_range){
  
  ## #######################################  
  
  rangle <- runif(nrow(selfoot), 0, 2*pi)
    
  rdist <- 1000 * runif(nrow(selfoot), dshift_range[1], dshift_range[2])
    
  rsad <- sample(c(-1,1), nrow(selfoot), replace=TRUE)
    
  rsam <- runif(nrow(selfoot), sca_range[1], sca_range[2])

  ## #######################################  
  
  simmids <- sf::st_coordinates(sf::st_centroid(selfoot))
  
  simmids[,1] <- simmids[,1] + (cos(rangle) * rdist)
  simmids[,2] <- simmids[,2] + (sin(rangle) * rdist)
     
  simareas <- sf::st_area(selfoot) * (1 + (rsad * rsam / 100))

  ## #######################################  
  
  simfoot_trans <- make_square_polygons(
        polymidpoints = simmids, 
        polyareas = simareas,
        polynames = paste0("simfoot", 1:nrow(selfoot)), 
        polycrs = sf::st_crs(selfoot))
  
  ## #######################################  
  
  simfoot_trans
}

## ###################################################

make_square_polygons <- function(polymidpoints, polyareas, polynames, polycrs){
    
    ## Create square footprint
    
    polyhalfwidths <- as.numeric(sqrt(polyareas)/2)
    
    simgeom <- as.list(NULL)
    
    for(k in 1:nrow(polymidpoints)){
        
        p1 <- c(polymidpoints[k,1] - polyhalfwidths[k], polymidpoints[k,2] - polyhalfwidths[k])
        p2 <- c(polymidpoints[k,1] + polyhalfwidths[k], polymidpoints[k,2] - polyhalfwidths[k])
        p3 <- c(polymidpoints[k,1] + polyhalfwidths[k], polymidpoints[k,2] + polyhalfwidths[k])
        p4 <- c(polymidpoints[k,1] - polyhalfwidths[k], polymidpoints[k,2] + polyhalfwidths[k])
        
        simgeom[[k]] <- sf::st_multipolygon(list(list(ox = rbind(p1, p2, p3, p4, p1))))
    }
    
    sf::st_sf(NAME = polynames, geom = sf::st_sfc(simgeom, crs = polycrs))
}

## ###################################################

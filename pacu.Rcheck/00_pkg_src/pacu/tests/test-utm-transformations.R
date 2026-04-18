require(pacu)
require(sf)

run.test <-  get(".run.local.tests", envir = pacu.options)

if(run.test){
  
  pts <- data.frame(x = c(-99.02, -93.02, -87.02), y = rep(42.09, 3))
  pts <- sf::st_as_sf(pts, coords = c('x', 'y'), crs = 'epsg:4326')
  
  crs.strs <- c()
  
  for (i in 1:nrow(pts)){
    a <- pa_2utm(pts[i, ])
    a <- st_crs(a)$input
    crs.strs <- c(crs.strs, a)
  }
  
  
}

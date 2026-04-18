require(pacu)
require(sf)

run.test <-  get(".run.local.tests", envir = pacu.options)

if(run.test){
  
  pt <- data.frame(x = c(-93.02), y = c(42.09))
  pt <- sf::st_as_sf(pt, coords = c('x', 'y'), crs = 'epsg:4326')
  pt <- sf::st_transform(pt, 32615)
  
  pts <- list()
  for ( i in 1:30) {
    p1 <- pt + i * 10
    pts <- append(pts, p1)
  }
  pts <- do.call('c', pts)
  st_crs(pts) <- st_crs(pt)
  
  pol <- pa_make_vehicle_polygons(pts[1],
                                  5,
                                  sqrt(200),
                                  45)
  
  
  
  pols1 <- pa_make_vehicle_polygons(pts,
                                    rep(5, 30),
                                    rep(sqrt(200), 30),
                                    rep(45, 30),
                                    cores = 1)
  
  pols2 <- pa_make_vehicle_polygons(pts,
                                    rep(5, 30),
                                    rep(sqrt(200), 30),
                                    rep(45, 30),
                                    cores = 2)
  
  
  pols <- pa_make_vehicle_polygons(pts,
                                   rep(5, 30),
                                   rep(sqrt(200), 30),
                                   rep(45, 30))
  cpols1 <- pa_adjust_obs_effective_area(pols,
                                         1:30,
                                         cores = 1)
  
  cpols2 <- pa_adjust_obs_effective_area(pols,
                                         1:30,
                                         cores = 2)
  
  
  
  
  pols <- pa_make_vehicle_polygons(pts,
                                   rep(5, 30),
                                   rep(30, 30),
                                   rep(45, 30))
  
  cpols <- pa_adjust_obs_effective_area(pols,
                                        1:30,
                                        cores = 1)
  area.original <- mean(as.numeric(sf::st_area(pols)))
  area.solved <- mean(as.numeric(sf::st_area(cpols)))
  
  
  
  
  
  angles <- pacu:::.pa_estimate_angle(pts)
  angles<- as.numeric(angles)
  
  
  
}

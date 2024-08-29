run.test <-  get(".run.local.tests", envir = pacu.options)

if(run.test){
  require(pacu)

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

  test_that('check functions that build polygons',
            {
              pol <- pa_make_vehicle_polygons(pts[1],
                                              5,
                                              sqrt(200),
                                              45)
              expect_s3_class(pol, 'sfc_POLYGON')
            })


  test_that('check paralellization when making polygons',
            {
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

              expect_identical(pols1, pols2)
            })

  test_that('check paralellization when chopping polygons',
            {
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
              expect_identical(cpols1, cpols2)
            })



  test_that('check that chopped polygons are actually smaller than initial polygons',
            {
              pols <- pa_make_vehicle_polygons(pts,
                                               rep(5, 30),
                                               rep(30, 30),
                                               rep(45, 30))

              cpols <- pa_adjust_obs_effective_area(pols,
                                                     1:30,
                                                     cores = 1)
              area.original <- mean(as.numeric(sf::st_area(pols)))
              area.solved <- mean(as.numeric(sf::st_area(cpols)))

              expect_lt(area.solved, area.original)
            })




  test_that('check function that estimates the angles',
            {
              angles <- .pa_estimate_angle(pts)
              angles<- as.numeric(angles)

              expect_identical(angles, rep(45, 30))
            })

}

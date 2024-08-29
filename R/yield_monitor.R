#' @title Adjust the effective area of each observation
#'   based on vehicular polygon overlap
#' @description Adjust the effective area of each
#'   observation based on vehicular polygon overlap
#' @name pa_adjust_obs_effective_area
#' @rdname pa_adjust_obs_effective_area
#' @param polygons sf object containing vehicular polygons
#' @param obs.vector a vector containing the observations
#' @param var.label a string used to label the columns
#'   (e.g., yield)
#' @param overlap.threshold a fraction threshold to remove
#'   observations. A value of 0 does not remove any
#'   observations. A value of 1 removes all observations
#'   that overlap even minimally with neighboring
#'   observations.
#' @param cores the number of cores used in the operation
#' @param verbose whether to print operation details
#' @details This function will make use of the vehicular
#'   polygons to evaluate the overlap between polygons and
#'   adjust the variable in obs.vector to the effective area
#'   in the polygon. This is primarely intended for yield.
#' @export
#'

pa_adjust_obs_effective_area <- function(polygons,
                                         obs.vector,
                                         var.label = 'yield',
                                         overlap.threshold = 0,
                                         cores = 1L,
                                         verbose = FALSE) {
  area.ratio <- NULL

  obs.df <- data.frame(obs = obs.vector,
                       id = 1:length(obs.vector))
  names(obs.df) <- c(paste0('obs.', var.label), 'id')
  chp.polygons <- .pa_solve_vehicle_overlap(polygons, cores = cores, verbose = verbose)
  chp.polygons <- st_as_sf(chp.polygons)
  chp.polygons$id <- 1:nrow(chp.polygons)
  chp.polygons <- as.data.frame(chp.polygons)
  chp.polygons <- merge(chp.polygons, obs.df, by = 'id')
  polygons <- st_as_sf(polygons)
  polygons$id <- 1:nrow(polygons)
  polygons$area.initial <- as.numeric(st_area(polygons))
  polygons <- as.data.frame(polygons)
  chp.polygons <- merge(chp.polygons, polygons[c('id', 'area.initial')], by = 'id')
  chp.polygons <- st_as_sf(chp.polygons)
  chp.polygons$area.chopped <- as.numeric(st_area(chp.polygons))
  chp.polygons$area.ratio <- chp.polygons$area.chopped / chp.polygons$area.initial
  a.ratios <- data.frame(adj.obs = chp.polygons[[paste0('obs.', var.label)]] / chp.polygons$area.chopped)
  names(a.ratios) <- paste0('adj.', var.label)
  chp.polygons <- cbind(chp.polygons, a.ratios)
  chp.polygons <- subset(chp.polygons, area.ratio >= overlap.threshold)

  return(chp.polygons)
}




#'
#' @title Make vehicular polygons for yield monitor
#'   observations
#' @description Make vehicular polygons for yield monitor
#'   observations
#' @name pa_make_vehicle_polygons
#' @rdname pa_make_vehicle_polygons
#' @param points a vector of points
#' @param swath a vector containing the swath of the vehicle
#'   in meters
#' @param distance a vector containing the distance traveled
#'   by the vehicle in meters
#' @param angle a vector containing the angle of the
#'   vehicle's trajectory. If not supplied, the function
#'   will attempt to estimate the trajectory angle using the
#'   geographical information contained in the georeferenced
#'   points/
#' @param cores the number of cores used in the operation
#' @param verbose whether to print operation details
#' @details This function will create vehicular polygons
#'   based on the distance between points, angle of the
#'   vehicle's trajectory, and swath.
#' @return an sf object
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \dontrun{
#' ## for examples, see vignette pacu
#' }
#'
pa_make_vehicle_polygons <- function(points, swath, distance, angle = NULL, cores = 1L, verbose = FALSE){

  if(!inherits(points, c("sf", "sfc", "sfg")))
    stop("Object 'points' should be of class 'sf', 'sfc' or 'sfg'", call. = FALSE)

  crt.crs <- sf::st_crs(points)
  is.utm <- grepl('UTM zone', crt.crs$wkt)
  if(!is.utm){
    stop("points should be in UTM")
  }

  if(is.null(angle)) {angle <- .pa_estimate_angle(points)}

  if (!is.null(attr(swath, 'units')))
    units(swath) <- NULL
  if (!is.null(attr(distance, 'units')))
    units(distance) <- NULL
  if (!is.null(attr(angle, 'units')))
    units(angle) <- NULL



  arg.lens <- sapply(list(points, swath, distance, angle), length)
  if(length(unique(arg.lens)) > 1)
    stop('all arguments should be of the same length', call. = FALSE)

  if(verbose)
    cat('Building', length(swath), 'polygons in', cores, 'cores.\n')
  if(cores != 1L){
    on.exit(parallel::stopCluster(cl))
    cores.avlb <- parallel::detectCores(logical = FALSE)

    ncores <- cores
    if (cores > cores.avlb){
      warning('Argument "cores" is greater than the number of available physical cores on the machine. Setting cores argument to ', cores.avlb,
              immediate. = TRUE)
      ncores <- cores.avlb
    }

    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(cl, c('.pa_make_vehicle_polygon', 'points', 'swath', 'distance', 'angle'), environment())
    parallel::clusterEvalQ(cl, {library('sf')})
    pols <- parallel::parLapply(cl,
                                1:length(swath),
                                function(i) {
                                  .pa_make_vehicle_polygon(points[i],
                                                           swath[i],
                                                           distance[i],
                                                           angle[i])
                                })

  }else{
    pols <- lapply(1:length(swath), function(i) {
      .pa_make_vehicle_polygon(points[i],
                               swath[i],
                               distance[i],
                               angle[i])})


  }
  pols <- do.call(c, pols)
  return(pols)
}

#'
#' @title Impose a regular grid over yield polygons
#' @description Impose a regular grid over yield polygons
#' @name pa_apportion_mass
#' @rdname pa_apportion_mass
#' @param polygons sf object containing polygon geometries
#' @param mass.vector a vector of mass observations
#' @param sum whether the apportioned values should be
#'   added together. This is useful in the case of
#'   overlaping polygons that have an additive effect. For
#'   example, polygons representing seeding rates.
#' @param cell.size optional numerical value (length 1) to
#'   be used as the width and height of the grid
#' @param remove.empty.cells logical. Whether to remove
#'   empty cells, with NA values.
#' @param cores the number of cores used in the operation
#' @param verbose whether to print operation details
#' @details This function will impose a regular grid over
#'   the yield polygons and compute the weighted average of
#'   the mass value represented by each polygon. The
#'   averages are weighted according to the polygon area.
#' @return  sf object
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \donttest{
#' ## for examples, see vignette pacu
#' }
#'
pa_apportion_mass <- function(polygons,
                              mass.vector,
                              cell.size = NULL,
                              sum = FALSE,
                              remove.empty.cells = TRUE,
                              cores = 1L,
                              verbose = FALSE){


  if(is.null(cell.size)){
    cell.size <-   2 * sqrt(stats::median(sf::st_area(polygons)))
    cell.size <- as.numeric(cell.size)
  }

  dat <- sf::st_as_sf(polygons)
  dat$mass <- mass.vector
  app.grid <- sf::st_make_grid(polygons, cellsize = rep(cell.size, 2))
  if(verbose) cat('Apportioning yield to', length(app.grid), 'polygons.\n')
  apported <- .pa_areal_weighted_average(dat, app.grid, 'mass', sf::st_intersects, sum, cores)

  if(remove.empty.cells)
    apported <- stats::na.omit(apported)

  return(apported)
}


#'
#' @title Reproject a sf object to UTM coordinates
#' @description Reproject a sf object to UTM coordinates
#' @name pa_2utm
#' @rdname pa_2utm
#' @param df sf object to be reprojected to UTM coordinates
#' @param verbose whether to print operation details
#' @details This function will attempt to automatically
#'   determine the adequate UTM zone and reproject a sf
#'   object,
#' @return a sf object
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \donttest{
#' ## for examples, see vignette pacu
#' }
#'
pa_2utm <- function(df, verbose = FALSE) {

  crt.crs <- sf::st_crs(df)
  is.utm <- grepl('UTM zone', crt.crs$wkt)

  if(is.utm){
    if(verbose) cat("Current CRS is already UTM\n")
    return(df)
  }
  ac <-try(sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(df)))),
           silent = TRUE)
  if (inherits(ac, 'try-error')) {ac <- sf::st_coordinates(df[1, ])}

  epsg.code <- .pa_coord2utm(lat = ac[2], long = ac[1])
  if(verbose) cat('Transforming CRS to EPSG:', epsg.code, '\n', sep = '')
  df <- st_transform(df, epsg.code)
  df
}




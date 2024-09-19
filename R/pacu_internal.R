#### Satellite data ----
#' Auxiliary unexported and \sQuote{hidden} functions
#' Retrieve cloud polygons from the zipped Sentinel 2 file
#' @name .pa_get_cloud_polygon
#' @description This function is used to retrieve the cloud mask from different sentinel products.
#' This is used in several steps to check for clouds over an area of interest.
#' @param sentinel_product the file path to a zipper sentinel file
#' @return a polygon of the clouds in the sentinel product
#' @noRd

.pa_get_cloud_polygon <- function(sentinel_product) {

  imgList <- utils::unzip(sentinel_product, list = TRUE)
  cloud_mask <- grep('B00\\.jp2|B00\\.gml', imgList[[1]], ignore.case = TRUE, value = TRUE)

  temporary_dir <- tempdir(check = TRUE)
  utils::unzip(sentinel_product, overwrite = TRUE, exdir = temporary_dir)

  ## The cloud mask can be in the gml format or jp2
  ## Here we check for the format because they have to be processed differently

  if (any(grepl('\\.gml', cloud_mask))) {

    doc <- XML::xmlTreeParse(file.path(temporary_dir, cloud_mask),
                             asTree = T)

    tolist <- rapply(doc, as.list)

    coords <- grep('^[0-9]{4,} .* [0-9]{4,}$', tolist, perl = TRUE, value = TRUE)

    if (length(coords) < 1){

      folders_to_delete <- dir(temporary_dir, pattern = '\\.SAFE')
      unlink(paste0(normalizePath(temporary_dir), "/", folders_to_delete), recursive = TRUE)

      return(NULL)}

    img_crs <- grep('crs:', tolist, perl = TRUE, value = TRUE)
    img_crs <- unlist(strsplit(img_crs, ':'))
    img_crs <- as.numeric(img_crs[length(img_crs)])

    res <- c()

    for (i in 1:length(coords)){
      numerical_coordinates <-  as.numeric(unlist(strsplit(coords[i], ' ')))
      coordinate_list <- split(numerical_coordinates, rep(c(1,2)/(length(numerical_coordinates) / 2 ) ))
      names(coordinate_list) <- c('lat','long')

      cloud_poly <-  sf::st_polygon(list(matrix(c(coordinate_list$lat, coordinate_list$long), 2)),
                                    dim = 'XY')

      cloud_poly <- sf::st_sfc(cloud_poly)

      cloud_poly  <- sf::st_set_crs(cloud_poly,  img_crs)

      res <- c(res, cloud_poly)
    }

    res <- sf::st_sfc(res)
    res  <- sf::st_set_crs(res,  img_crs)

    folders_to_delete <- dir(temporary_dir, pattern = '\\.SAFE')
  }

  if (any(grepl('\\.jp2', cloud_mask))) {
    clouds <- stars::read_stars(file.path(temporary_dir, cloud_mask))
    clouds <- sf::st_union(sf::st_geometry(sf::st_as_sf(clouds[clouds > 0, 1] + clouds[clouds > 0, 2])))
    res <- clouds
  }

  return(res)
}


#' Retrieve the Data Space token
#' @name .pa_get_dataspace_token
#' @description
#' This function retrieves the security token from Data Space. This is used in the
#' functions that download data from Data Space.
#' @param username username used to authenticate the request in Data Space
#' @param password password used to authenticate the request in Data Space
#' @noRd

.pa_get_dataspace_token <- function(username, password,
                                    oauth = FALSE
                                    ) {

  url  <-  "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"
  token_data <- list( client_id = "cdse-public", username = username,
                      password = password, grant_type = "password")

  if (oauth) {
    token_data <- list( client_id = username, client_secret = password,
                        grant_type = "client_credentials")
  }
  resp <- httr::POST( url,
                      body  = token_data,
                      encode = 'form')


  token <- httr::content(resp)
  token$timestamp <- Sys.time()

  if(is.null(token$access_token)) {
    stop('There was a problem when authenticating your credentials.
         Please make sure your user name and password are correct.')
  }

  return(token)
}

#' Crop a Sentinel 2 product to the area of interest
#' @name .pa_crop_s2_to_aoi
#' @description
#' This function crops all the raster files within the Sentinel 2 zipped file to
#' an area of interest
#' @param satellite_images list of file paths to be cropped to aoi
#' @param aoi sf object to which the file raster files will be cropped
#' @param img_formats image formats to search for in the zipped file
#' @noRd

.pa_crop_s2_to_aoi <- function(satellite_images,
                               aoi){

  for (sat_img in satellite_images) {

    image_indices <- .pa_select_s2_files(sat_img, which = 'images')
    temporary_dir <- tempdir(check = TRUE)
    bname <- basename(sat_img)
    bname <- strsplit(bname, '\\.')[[1]][1]
    temporary_dir <- file.path(temporary_dir, bname)
    dir.create(temporary_dir, showWarnings = FALSE, recursive = TRUE)
    utils::unzip(sat_img[[1]], overwrite = TRUE, exdir = temporary_dir, junkpaths = TRUE)
    cmi <- grep('B00', image_indices)
    cloud_mask <- image_indices[cmi]
    image_indices <- image_indices[-cmi]
    dir.create('./reduced/IMG_DATA', recursive = TRUE, showWarnings = FALSE)
    for (band in image_indices) {
      band_img <- stars::read_stars(file.path(temporary_dir, band))
      boundary <- sf::st_geometry(sf::st_transform(aoi, sf::st_crs(band_img)))
      band_img <- sf::st_crop(band_img, boundary, mask = TRUE)
      band <- gsub('\\.jp2', '\\.tif', band)

      img.out <- file.path('./reduced/IMG_DATA/', band)
      stars::write_stars(band_img,
                         img.out)
    }

    if (length(cloud_mask) > 0){
      file.copy(file.path(temporary_dir, cloud_mask),
                file.path('./reduced/IMG_DATA', cloud_mask))
    }

    metadata <- .pa_select_s2_files(fpath = sat_img, which = 'metadata')
    if (length(metadata) > 0){
      file.copy(file.path(temporary_dir, metadata),
                file.path('./reduced/IMG_DATA', metadata))
    }


    files_to_zip <- list.files('./reduced', recursive = TRUE)
    unlink(sat_img[[1]])
    utils::zip(sat_img[[1]],
               files = file.path('./reduced',
                                 files_to_zip),
               flags = '-q') ## might need to adjust this to different OS
    ## Deleting temporary folders
    folders_to_delete <- dir(temporary_dir, pattern = '[.]SAFE')
    unlink(paste0(normalizePath(temporary_dir), "/", folders_to_delete),
           recursive = TRUE)
    unlink('./reduced',
           recursive = TRUE)
  }
}

#' Retrieves the eval script
#' @name .pa_get_eval_script
#' @description
#' This function is used when building the json body to retrieve the javascript
#' file to be included in the json body
#' @param vegetation.index the vegetation index corresponding to the script to be used
#' @noRd

.pa_get_eval_script <- function(collection, vegetation.index) {
  js.dir <- system.file(paste0("js/", collection), package = "pacu")
  vegetation.index <- paste0(tolower(vegetation.index), '.js')
  index.src <- file.path(js.dir, vegetation.index)
  es <- paste(readLines(index.src), collapse = '\n')
  return(es)
}

#' Builds the request body for the statistics API
#' @name .build_request_body
#' @description
#' this function is used to build the json request body of the functions that interact
#' with the data space api
#' @param aoi area of interest for which the vegetation index will be requested
#' @param start_date beginning of the time window to request data
#' @param end_date end of the time window to request data
#' @param vegetation.index vegetation index to be calculated
#' @param agg.time time interval to request images
#' @noRd

.pa_build_request_body <- function(aoi,
                                   start.date,
                                   end.date,
                                   vegetation.index,
                                   agg.time,
                                   collection) {

  if(!inherits(aoi, 'sf')) {
    stop('aoi must be of class sf')
  }

  if(nrow(aoi) == 1){
    type <- 'Polygon'
  }else {
    type <- 'MultiPolygon'
  }

  geo_json_geometry <- list(type =  jsonlite::unbox(type),
                            coordinates = list())

  for(i in 1:nrow(aoi)){
    bbox <-  sf::st_cast(sf::st_geometry(aoi[i, ]), 'POINT')
    c1 <- sf::st_coordinates(bbox[1])
    epsg.code <- .pa_coord2utm(unname(c1[1]), unname(c1[2]))
    bbox <- sf::st_transform(bbox, epsg.code)
    if(type == 'Polygon'){

      geo_json_geometry[[2]][[i]] <- vector('list', length(bbox))

      for (j in 1:length(bbox)) {
        geo_json_geometry[[2]][[i]][[j]] <-  c(sf::st_coordinates(bbox[j]))
      }

    }else{
      geo_json_geometry[[2]][[i]] <- vector('list', 1)
      geo_json_geometry[[2]][[i]][[1]] <- vector('list', length(bbox))
      for (j in 1:length(bbox)) {
        geo_json_geometry[[2]][[i]][[1]][[j]] <-  c(sf::st_coordinates(bbox[j]))
      }
    }
  }

  geometry_filter <- list(
    bounds = list(geometry = c(geo_json_geometry),
                  properties =
                    list(crs = jsonlite::unbox(paste0("http://www.opengis.net/def/crs/EPSG/0/", epsg.code)))))

  vi.script <- jsonlite::unbox(.pa_get_eval_script(collection, vegetation.index))
  aggregation_filter <- list(
    timeRange = list(
      from= jsonlite::unbox(paste0(start.date,"T00:00:00Z" )),
      to= jsonlite::unbox(paste0(end.date,"T23:59:59Z" ))),
    aggregationInterval = list(of = jsonlite::unbox(agg.time)),
    evalscript = vi.script,
    resx = jsonlite::unbox(10),
    resy = jsonlite::unbox(10))

  data_filter <- list(
    data = list(
      list(
        type = jsonlite::unbox(collection),
        dataFilter = c()
      )
    )
  )

  calculations_filter <- list(default = list(
    statistics = list(default = list(percentiles = list(k = c(50)),
                                     interpolation = jsonlite::unbox('higher')))))

  filter_configs <- list(
    input = c(geometry_filter, data_filter),
    aggregation = aggregation_filter,
    calculations = calculations_filter)

  body_json <- jsonlite::toJSON(filter_configs,
                                pretty = TRUE,
                                digits = 12)
  return(body_json)
}

#'
#' @title Select files in the S2A and S2B products to crop the file
#' @description  Select files in the S2A and S2B products to crop the file
#' @name .pa_select_s2_files
#' @rdname .pa_select_s2_files
#' @param fpath a file path from which the S2A/S2B files will be select
#' @return a vector with relevant file names
#' @noRd

.pa_select_s2_files <- function(fpath,
                                which = c('all', 'images', 'metadata')) {

  which <- match.arg(which)
  imgList <- utils::unzip(fpath, list = TRUE)
  bname <- basename(fpath)

  cld.str <- 'B00\\.jp2|B00\\.gml'
  cloud_mask <- grep(cld.str, imgList[[1]], ignore.case = TRUE, value = TRUE)

  mtd <- grep('MTD_MSI', imgList[[1]], value = TRUE)

  if (length(mtd) < 1) {
    warning('No metadata found for ', basename(fpath), immediate. = TRUE)
  } else {mtd <- mtd[[1]]}

  if (grepl('^S2A|^S2B', x = bname)){
    rel.str <- paste0('IMG_DATA/.{1,}B[0-9]{2,2}.{0,}\\.(jp2|tif)')
    img.indices <- grep(rel.str, imgList[[1]], value = TRUE)
  #} else if(grepl('^S2B', x = bname)){
    #rel.str <- paste0('IMG_DATA/.{1,}B[0-9]{2,2}\\.(jp2|tif)')
    #img.indices <- grep(rel.str, imgList[[1]], value = TRUE)
  }else{
    stop('Only S2A and S2B functions are supported for now.')
  }

  if(which == 'all')
    rel.files <- basename(unlist(c(img.indices, cloud_mask, mtd)))

  if(which == 'images')
    rel.files <- basename(unlist(c(img.indices, cloud_mask)))

  if(which == 'metadata')
    rel.files <- basename(unlist(c(mtd)))
  return(rel.files)
}

#'
#' @title Retrieves the file names of a band
#' @description  Retrieves the file names of a band
#' @name .pa_get_band
#' @rdname .pa_get_band
#' @param band a string representing a band name
#' @param src.dir the directory in which to look for the band
#' @param pixel.res pixel resolution
#' @param extensions image extensions
#' @return filename of the selected band
#' @noRd

.pa_get_band <- function(band,
                         src.dir,
                         pixel.res = c('default', '10m', '20m', '60m'),
                         extensions = c('jp2', 'tif')) {
  extensions <- paste(extensions, collapse = '|')
  extensions <- paste0('\\.(', extensions, ')$')
  pixel.res <- match.arg(pixel.res)
  flist <- list.files(src.dir, recursive = TRUE, full.names = TRUE)

  if (pixel.res == 'default') pixel.res <- NULL

  bb <- paste0(band,'.{0,}', pixel.res, '.{0,}', extensions)
  ans <- grep(bb, flist, value = TRUE)

  if(length(ans) > 1)
    ans <- ans[1]
  if(length(ans) < 1)
    stop('Could not find band ', band)
  ans
}


#'
#' @title Reads the metadata from a S2 file
#' @description  Reads the metadata from a S2 file
#' @name .pa_read_s2_metadata
#' @rdname .pa_read_s2_metadata
#' @param fpath a file path pointing to an S2 file
#' @return a list containing the file metadata
#' @noRd
.pa_read_s2_metadata <- function(fpath) {
  mtd <- XML::xmlTreeParse(file = fpath)
  mtd <- XML::xmlToList(mtd)
  return(mtd)
}

## Weather ----
#' Convert the units in a met file to standard units
#' @name .pa_convert_met_to_standard
#' @description
#' This function is used in the weather reports to convert the units
#' of the met file to standard units.
#' @param weather.data an object of met class containing the data to be summarized
#' @noRd

.pa_convert_met_to_standard <- function(weather.data){
  weather.data$maxt <- with(weather.data, maxt <- maxt * 1.8 + 32)
  weather.data$mint <- with(weather.data, mint <- mint * 1.8 + 32)
  weather.data$rain <- with(weather.data, rain <- rain / 25.4)
  weather.data
}

## Yield Monitor ----
#' Convert lat/long to UTM EPSG Code
#' @name .pa_coord2utm
#' @description
#' This function is used to convert crs from lat/long to UTM
#' @param long longitude
#' @param lat latitude
#' @noRd

.pa_coord2utm <- function(long, lat) {
  utm.zone <- (floor((long + 180)/6) %% 60) + 1
  epsg.code <- ifelse(lat > 0, 32600 + utm.zone, 32700 + utm.zone)
  return(epsg.code)
}

#' Chops polygons to account for overlap
#' @name .pa_chop_polygons
#' @description
#' This function is used to chop overlapping polygons and evaluate the overlapped area
#' @param polygon a vector of vehicular polygons
#' @noRd

.pa_chop_polygons <- function(polygons,
                              use_s2 = TRUE){

  if(!use_s2){
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(TRUE)))
  }

  ## if there are less than two polygons, we can skip the function
  if (length(polygons) < 2){return(polygons)}

  ## process polygons in the reverse order
  polygons <- rev(polygons)
  pol.intersections <- sf::st_intersects(polygons, remove_self = TRUE)
  int.ps <- (1:length(polygons))[lengths(pol.intersections) >= 1]
  for (i in int.ps){
    crt.pol <- polygons[i]
    int.pols <- unlist(pol.intersections[i])
    if(length(int.pols) < 1) {next}
    nb.pols <- try(sf::st_union(polygons[int.pols]), silent = TRUE)
    chopped.pol <- try(sf::st_difference(crt.pol, nb.pols), silent = TRUE)
    if(length(chopped.pol) < 1 || inherits(chopped.pol, 'try-error')) {chopped.pol <- NULL}
    polygons[i] <- chopped.pol
  }
  ## reverse again
  polygons <- rev(polygons)
  polygons
}

#' Calculate a weighted mean based on area
#' @name .pa_areal_weighted_average
#' @description
#' This function will calculated weighted averages based on the area of overlap between x and y
#' based on a function that determines the intersection between polygons
#' @param x sf object containing the polygons from which the data will be extracted
#' @param y sf object containing the polygon to which the data will be averaged
#' @param fn a function that determines the overlap between polygons
#' @param cores number of cores to be used in the operation
#' @noRd

.pa_areal_weighted_average <- function(x, y, var, fn, sum = FALSE, cores = 1L){
  pol.intersections <- fn(y, x)
  int.ps <- (1:length(y))[lengths(pol.intersections) >= 1]
  y <- sf::st_geometry(y)
  y <- sf::st_as_sf(y)
  y[[var]] <- NA
  pol.list <- lapply(pol.intersections, function(is) x[unlist(is), ])

  if(cores > 1){
    on.exit(parallel::stopCluster(cl))
    cores.avlb <- parallel::detectCores(logical = FALSE)
    ncores <- cores
    if (cores > cores.avlb){
      warning('Argument "cores" is greater than the number of available physical cores on the machine. Setting cores argument to ', cores.avlb,
              immediate. = TRUE)
      ncores <- cores.avlb
    }
    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(cl, c('y', 'pol.list', 'var', 'sum'), environment())
    parallel::clusterEvalQ(cl, {library('sf')})
    avs <- parallel::parLapply(cl,
                               1:length(pol.list),
                               function(i) {
                                 ol.pols <- pol.list[[i]]
                                 ol.pols <- suppressWarnings(sf::st_intersection(ol.pols, sf::st_buffer(y[i, ], 0)))
                                 ol.pols$area <- as.numeric(sf::st_area(ol.pols))
                                 cov.frac <- sum(ol.pols$area)/ as.numeric(sf::st_area(sf::st_buffer(y[i, ], 0)))
                                 if (cov.frac < 0.25) { return(NULL)}
                                 if(sum) {
                                   wv <- as.numeric(sf::st_area(ol.pols)) * ol.pols[[var]]
                                   wv <- sum(wv)
                                   pol.mean <- wv / as.numeric(sf::st_area(sf::st_union(ol.pols)))
                                 }else{
                                   ol.pols <- as.data.frame(ol.pols)
                                   pol.mean <- stats::weighted.mean(ol.pols[[var]],
                                                                    ol.pols$area,
                                                                    na.rm = TRUE)
                                 }
                                 data.frame(i = i, z = pol.mean)
                               })

  }else{
    avs <- lapply(1:length(pol.list),
                  function(i) {
                    ol.pols <- pol.list[[i]]
                    ol.pols <- suppressWarnings(sf::st_intersection(ol.pols, sf::st_buffer(y[i, ], 0)))
                    ol.pols$area <- as.numeric(sf::st_area(ol.pols))
                    cov.frac <- sum(ol.pols$area)/ as.numeric(sf::st_area(sf::st_buffer(y[i, ], 0)))
                    if (cov.frac < 0.25) { return(NULL)}

                    if(sum) {
                      wv <- as.numeric(sf::st_area(ol.pols)) * ol.pols[[var]]
                      wv <- sum(wv)
                      pol.mean <- wv / as.numeric(sf::st_area(sf::st_union(ol.pols)))
                    }else{
                      ol.pols <- as.data.frame(ol.pols)
                      pol.mean <- stats::weighted.mean(ol.pols[[var]],
                                                       ol.pols$area,
                                                       na.rm = TRUE)
                    }

                    data.frame(i = i, z = pol.mean)
                  })
  }
  avs <- do.call(rbind, avs)
  y[[var]][avs$i] <- avs$z
  y
}

#' Solves vehicular overlap
#' @name .solve_vehicle_overlap
#' @description
#' This function is used to solve for vehicle overlap and chop polygons
#' @param polygons a vector of vehicular polygons
#' @param cores a numerical value indicating the number of cores to be used in this operation
#' @noRd

.pa_solve_vehicle_overlap <- function(polygons, cores = 1L, verbose = FALSE) {
  pol.intersections <- sf::st_intersects(polygons, remove_self = FALSE)
  number.of.conflicts <- sum(as.numeric(lengths(pol.intersections)  > 1))

  if(verbose){cat('Solving polygon boundaries of', number.of.conflicts, 'overlapping polygons in', cores, 'core(s)','\n')}

  pol.list <- lapply(pol.intersections, function(x) polygons[unlist(x)])
  tgt.pol <- lapply(1:length(pol.intersections), function(i) which(unlist(pol.intersections[i]) == i))

  if(number.of.conflicts > 0) {
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
      parallel::clusterExport(cl, c('.pa_chop_polygons', 'pol.list', 'tgt.pol'), environment())
      parallel::clusterEvalQ(cl, {library('sf')})
      chp.pols <- parallel::parLapply(cl,
                                      1:length(pol.list),
                                      function(i) {
                                        pols <- .pa_chop_polygons(pol.list[[i]])
                                        pols[tgt.pol[[i]]]
                                      })

    }else{
      chp.pols <- lapply(1:length(pol.list),
                         function(i) {
                           pols <- .pa_chop_polygons(pol.list[[i]])
                           pols[tgt.pol[[i]]]
                         })

    }

    chp.pols <- do.call(c, chp.pols)
    polygons <- chp.pols
  }
  polygons
}






#' Make vehicular polygon
#' @name .make_vehicle_polygon
#' @description
#' This function makes individual vehicular polygons based on location, swath, distance, and trajectory angle
#' @param point a point geometry
#' @param swath a swath value in meters
#' @param distance distance, in meters, between the previous point and the current
#' @param angle an angle in degrees
#' @noRd

.pa_make_vehicle_polygon <- function(point, swath, distance, angle){

  ## swath and distance must be in meters
  ## angle must be in degrees
  ## distance is the distance between the current point and the previous

  point <- sf::st_geometry(point)
  s <- swath / 2
  d1 <- distance /2
  a <- angle * pi / 180

  x0 <- st_coordinates(point)[1]
  y0 <- st_coordinates(point)[2]

  topleft <- c(x0 - s, y0)
  topright <- c(x0 + s, y0)
  bottomleft <- c(x0 - s, y0 - 2 * d1)
  bottomright <- c(x0 + s, y0 - 2 * d1)

  pts <- matrix(c(bottomleft,
                  bottomright,
                  topright,
                  topleft,
                  bottomleft),
                byrow = TRUE,
                ncol = 2)

  pol <- st_polygon(list(pts))
  pol <- sf::st_sfc(sf::st_polygon(list(pts)))
  rm <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  rpol <- (pol -point) * rm + point
  sf::st_crs(rpol) <- sf::st_crs(point)
  rpol
}

#'
#' @title Wrapper to fit a variogram
#' @description Wrapper to fit a variogram
#' @name .pa_fit_variogram
#' @param formula a formula object describing the trend
#' @param df an sf object containg the variables specified in the formular
#' @param robust whether to use Cressie's robust estimator
#' @details This function will fit and return a Mattern variogram given a formula and a sf object
#' @return returns an sf object
#' @noRd

.pa_fit_variogram <- function(formula, df, robust = TRUE, fun, verbose = FALSE) {
  if(verbose) cat('Fitting variogram \n')

  if(fun == 'log') {
    resp.col <- as.character(formula[[2]])
    df[[resp.col]] <- log(df[[resp.col]])
  }

  v1 <- gstat::variogram(formula,  df, cressie = robust)

  f1 <- try(suppressWarnings(gstat::fit.variogram(v1,
                                                  gstat::vgm(NA, gstat::vgm()$short[2:9], NA, NA),
                                                  fit.sills = TRUE,
                                                  fit.ranges = TRUE,
                                                  fit.kappa = TRUE)) , silent = TRUE)

  if (inherits(f1, 'try-error')) {
    stop('Failed to fit a variogram using the provided formula and data.')
  }

  list(f1, v1)
}

#'
#' @title Wrapper to paralellize the Krige function
#' @description Wrapper to paralellize the Krige function
#' @name .pa_predict
#' @param formula a formula object describing the trend
#' @param df an sf object containg the variables specified in the formula
#' @param new.df a new data frame for which predictions will be made
#' @param model a variogram model
#' @param cores number of cores to be used in the paralellization
#' @details This function will return krigged values
#' @return returns an sf object
#' @noRd

.pa_predict <- function(formula,
                        smooth.method,
                        df,
                        new.df,
                        cores = 1L,
                        fun,
                        verbose = FALSE,
                        ...){

  ## for now, we are limiting the number of cores to 1, as kriging paralellization has
  ## shown to be fatal in windows...
  #if (cores > 1){
  #  cores <- 1
  #}


  if (is.null(new.df)) {
    new.df <- df
  }

  # if (smooth.method == 'krige' && fun == 'log' && cores > 1){
  #   if(verbose)
  #     cat('Lognormal kriging is only supported using 1 core. Defaulting the smoothing process to 1 core.\n')
  #   #cores <- 1
  # }

  if(verbose) cat('Smoothing method: ', smooth.method,'\n',
                  'Number of features: ', nrow(new.df),'\n',
                  'Feature type: ', as.character(sf::st_geometry_type(new.df)[1]), '\n',
                  'Cores: ', cores, '\n', sep = '')

  if (smooth.method == 'krige'){
    model <- .pa_fit_variogram(formula, df, robust = TRUE, fun = fun, verbose = verbose)
    vari <- model[[2]]
    model <- model[[1]]


    if(cores > 1){
      if(verbose)
        cat('Kriging...\n')

      on.exit(parallel::stopCluster(cl))
      new.df$coreTo <- cut(1:nrow(new.df), cores, labels = FALSE)
      new.df <- split(new.df, new.df$coreTo)
      cores <- min(cores, parallel::detectCores())
      cl <- parallel::makeCluster(cores)
      parallel::clusterExport(cl, c('formula', 'df', 'new.df', 'model', 'fun'), environment())
      if (length(list(...))) {
      parallel::clusterExport(cl, c('...'), environment())
      }

      parallel::clusterEvalQ(cl, {library('sf')})
      preds <- parallel::parLapply(cl,
                                   new.df,
                                   function(pred.locs) {

                                     if(fun == 'log') {
                                       gstat::krigeTg(formula,
                                                      df,
                                                      pred.locs,
                                                      model,
                                                      lambda = 0,
                                                      ...)

                                     }else{

                                       gstat::krige(formula,
                                                    df,
                                                    pred.locs,
                                                    model,
                                                    ...)
                                     }



                                   })
      preds <- do.call(rbind, preds)

      if (fun == 'log'){
        preds <- preds[c('var1TG.pred', 'var1TG.var')]
        names(preds) <- c('var1.pred', 'var1.var', 'geometry')
      }

      preds <- list(preds, model, vari)
    }else{
      if(fun == 'log'){
        preds <- gstat::krigeTg(formula,
                                df,
                                new.df,
                                model,
                                lambda = 0,
                                debug.level = -1 * verbose,
                                ...)
        preds <- preds[c('var1TG.pred', 'var1TG.var')]
        names(preds) <- c('var1.pred', 'var1.var', 'geometry')
      } else {
        preds <- gstat::krige(formula,
                              df,
                              new.df,
                              model,
                              debug.level = -1 * verbose,
                              ...)
      }

      preds <- list(preds, model, vari)
    }
  }else if (smooth.method == 'idw'){
    preds <- gstat::idw(formula,
                        df,
                        new.df,
                        debug.level = -1 * verbose,
                        ...)
    preds <- list(preds)
  }
  preds
}

#'
#' @title Retrieve a vector given the data frame and the variable
#' @description Retrieve a vector given the data frame and the variable
#' @name .pa_get_variable
#' @param df an sf object containing the columns which to search for a variable
#' @param var a string indicating which variable to search for
#' @param verbose whether to print information the steps in this function
#' @details This function will return a vector containing the variable values and converted to the units needed for the ritas steps
#' @return a vector
#' @noRd

.pa_get_variable <- function(df, var, inf.unit, inf.col, verbose){

  if (is.na(inf.col)){
    inf.col <- .pa_get_variable_columns(df, var, verbose)
  }

  if (is.null(inf.col)){ return(NULL)}

  tgt <- df[[inf.col]]

  if (!is.na(inf.unit)) {
    units(tgt) <- units::as_units(inf.unit)
  }else {
    comparison.vector <- .pa_get_comparison_vector(df, var)
    tgt <- .pa_guess_units(tgt, var, comparison.vector,verbose)
  }
  tgt <- .pa_enforce_units(tgt, var)
  tgt
}

#'
#' @title retrieve a vector with known units from the georeferenced information in the 
#' data to help with unit guessing
#' @description retrieve a vector with known units from the georeferenced information in the 
#' data to help with unit guessing
#' @name .pa_get_comparison_vector
#' @param df an sf object containing the columns which to search for a variable
#' @param var a string indicating which variable to search for
#' @return a vector
#' @noRd
.pa_get_comparison_vector <- function(df, var){
  comparison.vector <- NULL
  
  if (var == 'distance') {
    ## setting this comparison vector to 10% of the 
    ## length of the data. We can possibly set this
    ## to a global variable
    sample.size <- nrow(df) %/% 10
    comp.df <- sf::st_geometry(df)
    comp.df <- utils::head(comp.df, sample.size)
    comparison.vector <- sf::st_distance(comp.df[1:(sample.size-1), ],
                                         comp.df[2:sample.size, ],
                                         by_element = TRUE)
  }  
  
  return(comparison.vector)
  
}

#'
#' @title Retrieve a column name given the data frame and the variable
#' @description Retrieve a column name given the data frame and the variable
#' @name .pa_get_variable_columns
#' @param df an sf object containing the columns which to search for a variable
#' @param var a string indicating which variable to search for
#' @param verbose whether to print information the steps in this function
#' @details This function will return a vector containing the variable values and converted to the units needed for the ritas steps
#' @return a vector
#' @noRd
.pa_get_variable_columns <- function(df, var, verbose){

  tgt.col <- .pa_get_variable_names(var)
  tgt.col <- factor(tgt.col, levels = tgt.col, ordered = TRUE)

  tgt.index <- which(tolower(names(df)) %in% tgt.col)
  if(length(tgt.index) < 1) {
    if (verbose) warning('Unable to find the column for ', var, '\n',
                         immediate. = TRUE)
    return(NULL)
  }
  tgt.names <- names(df)[tgt.index]

  if(length(tgt.names) > 1){
    tgt.name <- tgt.names[tolower(tgt.names) == as.character(min(tgt.col[tgt.col %in% tolower(tgt.names)]))]
    if(verbose){
      warning('Columns ', paste(tgt.names, collapse = ', '), ' matched the variable ', var,
              '. Returning column ', tgt.name, '.\n', sep = '')
    }
  } else {
    tgt.name <-  tgt.names
  }

  return(tgt.name)

}



#'
#' @title Retrieve the interval between measurements from the time column
#' @description Retrieve the interval between measurements from the time column
#' @name .pa_time2interval
#' @param x a vector with the time data
#' @param fmts.to.try a vector containing strings of formats to try.
#' @details This function will convert datetime to interval
#' @return a vector
#' @noRd
.pa_time2interval <- function(x,
                              fmts.to.try =c("%Y-%m-%dT%H:%M:%OSZ",
                                             "%Y-%m-%d %H:%M:%OS",
                                             "%Y/%m/%d %H:%M:%OS",
                                             "%m-%d-%Y %H:%M:%OS",
                                             "%m/%d/%Y %H:%M:%OS",
                                             "%d-%m-%Y %H:%M:%OS",
                                             "%d/%m/%Y %H:%M:%OS",
                                             "%Y-%m-%d %H:%M",
                                             "%Y/%m/%d %H:%M",
                                             "%m-%d-%Y %H:%M",
                                             "%m/%d/%Y %H:%M",
                                             "%d-%m-%Y %H:%M",
                                             "%d/%m/%Y %H:%M",
                                             "%Y-%m-%d",
                                             "%Y/%m/%d",
                                             "%m-%d-%Y",
                                             "%m/%d/%Y",
                                             "%d-%m-%Y",
                                             "%d/%m/%Y",
                                             "%H:%M:%OSZ",
                                             "%H:%M:%OS",
                                             "%I:%M:%S %p",
                                             "%I:%M:%S%p"
                                             )){


  if (!inherits(x, c('character', 'POSIXlt', 'POSIXt'))) {
    stop('x must be either a character or a datetime object.')
  }

  if (is.character(x)){
  x <- as.POSIXlt(x, tryFormats = fmts.to.try)
  }

  if(inherits(x, c('POSIXlt', 'POSIXt'))){
    interval <- diff(x, units = 'secs')
    interval <- c(stats::median(interval), interval)
  }

  interval <- as.numeric(interval)

  if (length(interval[interval < 0]) > 0) {
    warning('Negative interval values detected when estimating interval from time. Please check the time column.')

  }


  return(interval)

}


#'
#' @title Enforce the units necessary for the RITAS steps
#' @description Enforce the units necessary for the RITAS steps
#' @name .pa_enforce_units
#' @param vec a vector of class "units"
#' @param var a string indicating which variable the vector represents
#' @details This function will enforce the units necessary for the ritas steps, for example meters and g m-2
#' @return a vector
#' @noRd

.pa_enforce_units <- function(vec, var){

  g <- s <- l <- count <- degreeN <- m <- NULL

  if (var == 'flow'){
    if(units::ud_are_convertible(units(vec), 'g/s')){
      ## flow mass
      units(vec) <- units::make_units(g/s)
    }else  if(units::ud_are_convertible(units(vec), 'l/s')){
      ## flow is volume
      units(vec) <- units::make_units(l/s)
    }else {
      ## flow is count
      units(vec) <- units::make_units(count/s)
    }
  }

  if (var == 'moisture'){
    units(vec) <- units::as_units(quote('%'))
  }

  if (var == 'angle') {
    units(vec) <- units::make_units(degreeN)
  }

  if (var == 'interval') {
    units(vec) <- units::make_units(s)
  }

  if (var == 'width') {
    units(vec) <- units::make_units(m)
  }

  if (var == 'distance') {
    units(vec) <- units::make_units(m)
  }
  vec
}


#'
#' @title Guess the units of a variable based on the values
#' @description Guess the units of a variable based on the values
#' @name .pa_guess_units
#' @param vec a vector of class \sQuote{units}
#' @param var a string indicating which variable the vector represents
#' @param comparison.vector a vector of values in metric units that can be extracted from the 
#' data to help guess the units of the dataset
#' @param verbose whether to print information on this functions steps
#' @details This function will attempt to guess the units of a variable based on normal ranges.
#' I intend to improve this function in the future.
#' @return a \sQuote{units} vector
#' @noRd

.pa_guess_units <- function(vec, var, comparison.vector = NULL, verbose = FALSE) {

  vs <- summary(vec)
  mu <- 'unknown'

  if(var == 'angle') {
    mu <- 'degreeN'
  }

  if(var == 'width') {
    if(vs[3] > 65) mu <- 'in'
    if(vs[3] < 65) mu <- 'ft' ## few combines are above 65ft
    if (vs[3] < 19) mu <- 'm' ## 65 ft is close to 19m also
  }

  if (var == 'interval') {
    mu <- 's'
  }

  if (var == 'flow') {
    if (vs[3] > 0 && vs[3] < 100) mu <- 'lb/s'
  }

  if (var == 'moisture') {
    mu <- '%'
  }

  if (var == 'mass') {
    if(vs[3] >= 1000) mu <- 'g'
    if(vs[3] < 2) mu <- 'kg'
    if(vs[3] > 2 && vs[3] < 1000) mu <- 'lb'

  }

  if (var == 'distance') {
    
    if (is.null(comparison.vector)){
      if (vs[3] < 5) mu <- 'm'
      if (vs[3] >= 5 && vs[3] <= 50) mu <- 'ft'
      if(vs[3] > 50) mu <- 'in'
    }else{
      comparison.vector <- as.numeric(comparison.vector)
      possible.units <- c('m', 'in', 'cm', 'ft')
      different.units <- sapply(possible.units, function(unt) {
        gu <- as.numeric(vs[3])
        units(gu) <- units::as_units(unt)
        units(gu) <- units::as_units('m')
        gu
      })
      unit.ratios <- abs((stats::median(comparison.vector)/ different.units) - 1) 
      guessed.unit <- which.min(unit.ratios)
      mu <- names(guessed.unit)
    }
  }
  
  if(var == 'yield') {
    if (vs[3] < 500 && vs[3] > 20) mu <- 'bushel/acre'
    if (vs[3] <= 20) mu <- 't/ha'
    if (vs[3] >= 500) mu <- 'kg/ha'
  }

  if(mu == 'unknown') {stop('unable to dermine the units of ', var, call. = FALSE)}

  message('Guessing units of ', var, ' to be ', mu)

  units::set_units(vec, mu, mode = 'standard')
}

#'
#' @title Search for a variable name in a dictionary containing possible names common in yield monitors
#' @description Search for a variable name in a dictionary containing possible names common in yield monitors
#' @name .pa_get_variable_names
#' @param vartype a variable type to retrieve names from the dictionary
#' @details this function will return a vector of common names for a given variable
#' @return a vector of possible names
#' @noRd

.pa_get_variable_names <- function(vartype = c('yield', 'angle', 'width',
                                               'interval', 'flow', 'distance',
                                               'moisture', 'time', 'mass')) {
  vartype <- match.arg(vartype)
  extd.dir <- system.file("extdata", package = "pacu")
  var.dict <- jsonlite::read_json(file.path(extd.dir, 'variable-names.json'))
  var.dict <- sapply(var.dict, function(x) unlist(x))
  var.indices <- sapply(var.dict, function(x) x['column'] == vartype )
  var.dict2 <- var.dict[var.indices]
  var.names <- lapply(var.dict2, function(x) {x[grepl('names', names(x))]})
  var.names <- unlist(unname(var.names))
  var.names <- unique(var.names)
  unlist(var.names)
}

#'
#' @title Estimate the angle between successive points
#' @description Estimate the angle between successive points
#' @name .pa_estimate_angle
#' @param points a vector of points
#' @details This function will estimate the angle of the trajectory between successive points
#' @return returns an sf object
#' @noRd

.pa_estimate_angle <- function(points){

  if(!inherits(points, c("sf", "sfc", "sfg")))
    stop("Object 'points' should be of class 'sf', 'sfc' or 'sfg'", call. = FALSE)

  angles <- c()
  for(i in 2:length(points)){
    # pt1 <- sf::st_coordinates(points[i - 1])
    # pt2 <- sf::st_coordinates(points[i])
    # vec1 <- c(0, pt2[2] - pt1[2])
    # vec2 <- c(pt2[1] - pt1[1], pt2[2] - pt1[2])
    # dot.prod <- vec2%*%vec1
    # m.det <- as.numeric(det(matrix(c(vec2, vec1), nrow = 2)))
    # a <- atan2( m.det, dot.prod)
    # a <- ifelse(a < 0, a + 2 * pi, a)
    # a <- a * 180 / pi
    # a <- as.numeric(a)
    #

    pt1 <- sf::st_coordinates(points[i - 1])
    pt2 <- sf::st_coordinates(points[i])
    a <- atan2(pt2[1] - pt1[1] , pt2[2] - pt1[2]) * 180 / pi
    a
    angles <- c(angles, a)
  }
  angles <- c(angles[1], angles)
  units(angles) <- units::as_units('degreeN')
  angles
}

#'
#' @title Estimate field boundary from points
#' @description Estimate field boundary from points
#' @name .pa_field_boundary
#' @param points a point geometry vector
#' @param arc arc of a circle to scan when building the concave hull around the field. Defaults to 5.
#' @param nsamples the number of polygons to be sampled for distances between consecutive and perpendicular points.
#' This is only relevant when method is 'polygons'.
#' @param method either 'distances' or 'polygons'. The distance method chooses the farthest points from the center for every arc-circle.
#' The polygon method will sample points from distances between consecutive and perpendicular points. The created polygons are then checked for overlap with
#' recorded points. The polygon method can identify holes but it is slower.
#' @details This function will estimate the field boundary by calculating the distance between the
#' convex hull and each of the point. The closest points to the convex hull for every arc-circle are
#' selected to define the boundary.
#' @return returns an sf object
#' @noRd

.pa_field_boundary <- function(points,
                               arc = 5,
                               nsamples = 300,
                               method = c('concaveman', 'distances', 'polygons'),
                               verbose = FALSE) {

  # crt.crs <- sf::st_crs(points)
  # is.utm <- grepl('UTM zone', crt.crs$wkt)
  # if(!is.utm){
  #   stop("points should be in UTM.")
  # }

  method <- match.arg(method)


  if (method == 'distances'){
    vex <- sf::st_convex_hull(sf::st_combine(points))
    center <- sf::st_centroid(vex)
    vex <- sf::st_cast(vex, 'LINESTRING')

    angles <- sapply(1:length(points),
                     function(i) {
                       pt1 <- sf::st_coordinates(center)
                       pt2 <- sf::st_coordinates(points[i])
                       a <- atan2(pt2[1] - pt1[1] , pt2[2] - pt1[2]) * 180 / pi
                       a
                     })

    pts.sf <- sf::st_as_sf(points)
    pts.sf$dists <- sf::st_distance(pts.sf, vex)
    pts.sf$angle <- angles
    pts.sf$ac <- cut(pts.sf$angle, breaks = seq(-180, 180, arc))

    pts.sf <- by(pts.sf, pts.sf$ac,
                 function(df) {
                   df <- df[order(df$dists), ]
                   df[1, ]
                 })

    pts.sf <- do.call(rbind, pts.sf)
    conc <- sf::st_cast(sf::st_geometry(sf::st_combine(pts.sf)), 'LINESTRING')
    conc <- sf::st_cast(conc, 'POLYGON')

  }

  if (method == 'polygons'){

    if(nsamples > length(points)) {nsamples <- length(points) - 1}
    smp.points <- sample(1:length(points) - 1, nsamples)
    dists.front <- sf::st_distance(points[smp.points], points[smp.points + 1], by_element = TRUE)
    close.points <- sf::st_intersects(sf::st_buffer(points[smp.points],  5 * stats::median(dists.front)), points)
    close.points <- lapply(close.points, unlist)
    perp.distances <- c()
    for (i in seq_along(smp.points)){
      if (length(close.points[[i]]) < 1) {next}
      crt.point <- points[smp.points[i]]
      angles <- sapply(close.points[[i]], function(j) .pa_estimate_angle(c(crt.point,
                                                                           points[j]))[1])
      perp.point <- which(abs(abs(angles) - 90)  < 5)
      if(length(perp.point) > 1){
        pd <- sf::st_distance(points[smp.points[i]], points[close.points[[i]][perp.point]])
        perp.distances <- c(perp.distances, pd)
      }
    }
    df <- as.numeric(stats::median(dists.front))
    dp <- as.numeric(stats::median(perp.distances)) / 2

    vex <- sf::st_convex_hull(sf::st_combine(points))
    grd <- sf::st_make_grid(vex, cellsize = rep((c(dp, df)), 2))

    cp <- sf::st_intersects(grd, points)
    cp <- sapply(cp, function(x) length(x) > 0)
    grd <- grd[cp]

    conc <- sf::st_union(sf::st_buffer(grd, max(c(dp, df)), endCapStyle = 'SQUARE', joinStyle = 'BEVEL'))
    conc <- sf::st_intersection(vex, conc)
  }



  if (method == 'concaveman') {
    if(!requireNamespace('concaveman', quietly = TRUE)){
     stop('The concaveman package is required to generate boundaries automatically.',
          ' You can install it with "install.packages("concaveman")".')
    }

    pts <-  sf::st_as_sf(sf::st_combine(points))
    conc <- concaveman::concaveman(pts)
    conc <- sf::st_zm(conc)
    conc <- sf::st_geometry(conc)
  }


  return(conc)
}



#'
#' @title Clean yield monitor data
#' @description Clean yield monitor data
#' @name .pa_clean_yield_monitor
#' @rdname .pa_clean_yield_monitor
#' @param df an sf object
#' @param tgt.col the target column to use when cleaning the data based on standard deviations
#' @param boundary an sf object representing the field edges
#' @param edge.threshold a numerical value. Observations closer to the edge of the field than this value will be removed.
#' @param sd.filter a numerical value used to remove yield observations that are farther from the mean than this value.
#' @param verbose whether to print information on this functions steps
#' @details This function will clean the yield monitor data based on proximity to the edge of the field and global outliers.
#' @return returns an sf object
#'
#' @noRd
#'
.pa_clean_yield_monitor <- function(df,
                                    tgt.col,
                                    boundary,
                                    edge.threshold = 30,
                                    sd.filter = 3,
                                    verbose = FALSE){

  edge.distance <- NULL


  initial.obs <- nrow(df)

  crt.crs <- sf::st_crs(df)
  if(is.na(crt.crs)) {
    if(verbose)
      cat('No CRS found. Defaulting to EPSG:4326\n')
    sf::st_crs(df) <- 'epsg:4326'
  }

  df <- pa_2utm(df, FALSE)


  crt.crs <- sf::st_crs(boundary)
  if(is.na(crt.crs)) {
    if(verbose )
      cat('No CRS found. Defaulting to EPSG:4326\n')
    sf::st_crs(boundary) <- 'epsg:4326'
  }


  boundary <- sf::st_transform(boundary, sf::st_crs(df))

  if(sf::st_geometry_type(boundary) %in% c('POLYGON', 'MULTIPOLYGON')) {
    boundary <- sf::st_cast(sf::st_geometry(boundary), 'MULTILINESTRING')
  }

  points <- suppressWarnings(sf::st_centroid(df))
  df$edge.distance <- as.numeric(st_distance(points, boundary))
  tgt <- df[[tgt.col]]
  df <- subset(df, edge.distance > edge.threshold)
  mean.tgt <- mean(tgt)
  sd.tgt <- stats::sd(tgt)
  upper.b <- mean.tgt + sd.filter * sd.tgt
  lower.b <- mean.tgt - sd.filter * sd.tgt
  df <- subset(df, df[[tgt.col]] > lower.b &
                 df[[tgt.col]] < upper.b &
                 df[[tgt.col]] != 0)

  final.obs <- nrow(df)

  if(verbose ) cat('A total of', initial.obs - final.obs, ' observations were removed during the cleaning process\n')
  df <- df[tgt.col]
  df
}


#'
#' @title Adjust for the time lag between the crop being cut by the combine and the sensor recording a data point
#' @description Adjust for the time lag between the crop being cut by the combine and the sensor recording a data point
#' @name .pa_adjust_lag
#' @param input an sf object containing the input data from a yield monitor
#' @param time.adj time, in seconds, to used to adjust the observations
#' @param time.col time columns in the input
#' @details This function will adjust for the time lag between the crop being cut by the combine and the sensor recording
#' a data point.
#' @return returns a sf object
#' @author Caio dos Santos and Fernando Miguez
#' @noRd
#'
.pa_adjust_lag <- function(input, time.adj, time.col) {

  time <- NULL

  if (time.adj > 0){
    input$time <- cumsum(input[[time.col]])
    pts <- st_geometry(input)
    idf <- as.data.frame(input)
    idf <- idf[-grep('geometry', names(idf))]
    idf <- subset(idf, time > time.adj)
    obs.shift <- dim(input)[1] - dim(idf)[1]
    shifted <- pts[1:(length(pts) - obs.shift) ]
    shifted <- cbind(idf, shifted)
    shifted <- st_as_sf(shifted)
    return(shifted)
  }else{
    return(input)
  }
}

#'
#' @title Convert crop flow to mass
#' @description Convert crop flow to mass
#' @name .pa_flow2mass
#' @param crop.flow a vector of crop flow in lb/s
#' @param interval a vector the interval between measurements in seconds
#' @param moisture vector of the recorded grain moisture in \%
#' @details This function will convert crop flow in lb/s to dry mass in g
#' @return returns a vector containing mass, in grams
#' @author Caio dos Santos and Fernando Miguez
#' @noRd
#'
.pa_flow2mass <- function(crop.flow, interval, moisture){
  ## crop.flow must be in g/s
  ## interval in seconds
  ## moisture in %
  mass <- crop.flow * interval
  attributes(mass) <- NULL
  attributes(moisture) <- NULL
  mass <- .pa_moisture(mass, moisture, 0)
  mass
}
#'
#' @title Retrieve the kriging weights from the kriging process
#' @description Retrieve the kriging weights from the kriging process
#' @name .pa_kriging_weights
#' @rdname .pa_kriging_weights
#' @param x an sf object
#' @param formula a formula object describing the trend
#' @param new.data a new data frame for which predictions will be made
#' @param model a variogram model
#' @details This function will retrieve the kriging weights from an object. It was not written by me. I found it in one of the gstat vignettes and thought it
#' could be useful to diagnose problems.
#' @return returns an sf object
#'
#' @noRd
#'
.pa_kriging_weights <- function(x, formula, newdata, model, ...) {
  weighti <- function(x, i, formula,...) {
    ret <- rep(0,nrow(x))
    ret[i]<-1
    x[[1]]<-ret
    gstat::krige(formula = formula,locations = x,...)
  }
  ret <- lapply(1:nrow(x), weighti, x=x, newdata=newdata[1,], model=model,formula=formula, ...)
  ret <- do.call(rbind, ret)
  unlist(ret[[1]])
}



#'
#' @title Retrieve the kriging weights from the kriging process
#' @description Retrieve the kriging weights from the kriging process
#' @name .available_memory
#' @details This function will retrieve the number of ram in byte.
#' @return available RAM in bytes
#'
#' @noRd
#'
.available_memory <- function()
{

  ## I did not write this function, I found it here:
  ## https://stackoverflow.com/questions/6457290/how-to-check-the-amount-of-ram
  ## I believe it can be useful for diagnosing code



  # Get operating system
  OS <- tolower(Sys.info()["sysname"])

  # Branch based on OS
  if(OS == "windows"){ # Windows

    # System information
    system_info <- system("systeminfo", intern = TRUE)

    # Get available memory
    value <- system_info[
      grep("Available Physical Memory", system_info)
    ]

    # Remove extraneous information
    value <- gsub("Available Physical Memory: ", "", value)
    value <- gsub("\\,", "", value)

    # Convert to bytes
    value_split <- unlist(strsplit(value, split = " "))

    # Check for second value
    bytes <- as.numeric(value_split[1]) * switch(
      value_split[2],
      "KB" = 1e03,
      "MB" = 1e06,
      "GB" = 1e09
    )

  }else if(OS == "linux"){ # Linux

    # Split system information
    info_split <- strsplit(system("free", intern = TRUE), split = " ")

    # Remove "Mem:" and "Swap:"
    info_split <- lapply(info_split, function(x){gsub("Mem:", "", x)})
    info_split <- lapply(info_split, function(x){gsub("Swap:", "", x)})

    # Get actual values
    info_split <- lapply(info_split, function(x){x[x != ""]})

    # Bind values
    info_split <- do.call(rbind, info_split[1:2])

    # Get free values
    bytes <- as.numeric(info_split[2, info_split[1,] == "free"])

  }else{ # Mac

    # System information
    system_info <- system("top -l 1 -s 0 | grep PhysMem", intern = TRUE)

    # Get everything after comma
    unused <- gsub(" .*,", "", system_info)

    # Get values only
    value <- gsub("PhysMem: ", "", unused)
    value <- gsub(" unused.", "", value)

    # Check for bytes
    if(grepl("M", value)){
      bytes <- as.numeric(gsub("M", "", value)) * 1e06
    }else if(grepl("G", value)){
      bytes <- as.numeric(gsub("G", "", value)) * 1e09
    }else if(grepl("K", value)){
      bytes <- as.numeric(gsub("K", "", value)) * 1e03
    }

  }

  # Return bytes
  return(bytes)

}



## Units ----

.pa_unit_system <- function(x, us, conv.fac, p = 1){
  if (us == 'standard') {
    if (is.null(conv.fac)){
      stop('"conv.fac" argument is necessary to convert units to US standard unit system.')
    }
    x <- .pa_bushel_metric(x, conv.fac, to = 'standard', p)
    units(x) <- (units::as_units(('bushel/acre')) ^ p)
  }
  if (us == 'metric') {
    x <- x * (1e4 ^ p) / (1e6 ^ p)
    units(x) <- (units::as_units('t/ha') ^ p)
  }
  x
}


.pa_bushel_metric <- function(x,
                              conv.fac,
                              to = c('standard', 'metric'),
                              p = 1 ) {

  ##conv.fac is the conversion factor in lb/bu

  to <- match.arg(to)

  ## if we want to go to bushel, I will assume we are always coming from
  ## grams/m2
  if (to == 'standard') {
    x <- 4046^p * x / (454^p * conv.fac^p) ## x will go from g/m2 to bu/ac
  }
  ## if we want to go to metric, I will assume we are always going to grams/m2
  if(to == 'metric'){
    x <- (x * conv.fac * 454) / (4046)  ## x will go from bushel/ac to gram/m2
  }
  x
}


.pa_moisture <- function(x, crt, to,
                         verbose = FALSE) {

  if (length(crt) == 1) {crt <- rep(crt, length(x))}
  if (length(to) == 1) {to <- rep(to, length(x))}

  if (verbose ) cat('Adjusting moisture to', round(to[1], 2), '%\n')
  x <- x * (100 - crt) / (100 - to)

  x
}


.pa_check_polygon_overlap <- function(input,
                                      n = 500){

  input <- utils::head(input, n)

  crt.crs <- sf::st_crs(input)
  if(is.na(crt.crs)) {
    sf::st_crs(input) <- 'epsg:4326'
  }

  input <- pa_2utm(input, FALSE)
  distance <- suppressMessages(.pa_get_variable(input, 'distance', NA, NA, FALSE))
  swath <- suppressMessages(.pa_get_variable(input, 'width', NA, NA, FALSE))
  angle <- suppressMessages(.pa_get_variable(input, 'angle', NA, NA, FALSE))
  if(is.null(angle)) {
    angle <- .pa_estimate_angle(sf::st_geometry(input))
  }

  swath <- units::drop_units(swath)
  distance <- units::drop_units(distance)
  angle <- units::drop_units(angle)

  pols <- pa_make_vehicle_polygons(points = sf::st_geometry(input),
                                   swath = swath,
                                   distance = distance,
                                   angle = angle)

  c.pols <- pa_adjust_obs_effective_area(pols,
                                         1:length(pols))
  stats::median(c.pols$area.ratio, na.rm = TRUE)
}

## Plotting ----


## Output ----

.pa_print_table <- function(x,
                            headers = TRUE,
                            frame = TRUE,
                            sep = ' ',
                            width = NULL,
                            digits = 3){


  x1 <- x
  x1.num <- suppressWarnings(apply(x1, 1:2, function(y){try(!is.na(as.numeric(y)), silent = TRUE)} ))
  x1[x1.num] <- floor(as.numeric(x[x1.num]))

  max.char <- apply(x1, 2, function(y) max(nchar(y)))
  max.char <- ifelse(nchar(names(x)) > max.char, nchar(names(x)), max.char)
  max.char <- unlist(unname(max.char))

  if (!is.null(width)){

  if(length(width) == 1) { width <- rep(width, length(max.char))}
  if(length(width) != length(max.char)){stop('Width must be length one or same as number of columns')}
  width <- ifelse(!is.na(width), width, max.char)
  max.char <- width

  }

  frame.width <- sum(max.char) + floor(1.5 * length(max.char))

  if(frame)
    cat(rep('-', frame.width), sep = '', fill = TRUE)

  if(headers){
    hh <- names(x)
    ho <- data.frame()
    ho[1, 1:length(hh)] <- hh
    ho[2, 1:length(hh)] <- rep('---', length(hh))
    names(ho) <- names(x)
    x <- rbind(ho, x)

  }

  for(i in 1:nrow(x)){
    row.out <- x[i, ]
    row.out <- unlist(row.out)

    to.print <- list()
    for (j in 1:length(max.char)){

      entry <- row.out[j]
       can.be.numeric <- suppressWarnings(try(as.numeric(entry), silent = TRUE))
       if (!inherits(can.be.numeric, 'try-error') && !is.na(can.be.numeric)){
         entry <- as.numeric(entry)

         if (entry %% 1 == 0) {ff <- 'd'}else {ff <- 'f'}

         dd <- max(max.char[j] - nchar(floor(entry) - 1), 0)

       }

      to.print[[j]] <- formatC(entry,
                               width = max.char[j] + 1,
                               format = ff,
                               digits = dd,
                               flag = '-')

    }
    cat(unlist(to.print), fill = TRUE, sep = sep)
  }

  if(frame)
    cat(rep('-', frame.width), sep = '', fill = TRUE)

}

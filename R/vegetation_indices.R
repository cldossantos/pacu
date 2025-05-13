#'
#' @title Compute vegetation indices from a zipped Sentinel
#'   2 file
#' @description Compute vegetation indices from a zipped
#'   Sentinel 2 file.
#' @name pa_compute_vi
#' @rdname pa_compute_vi
#' @param satellite.images list of file paths to the
#'   Sentinel 2 zip files
#' @param vi the vegetation index to be computed
#' @param aoi NULL or an sf object used to crop the
#'   vegetation index raster to an area of interest
#' @param formula an optional two-sided formula with the vegetation
#' index name on the left side and the relationship between the bands
#' on the right side. See example.
#' @param check.clouds whether to check for clouds over the
#'   area of interest. If clouds are found, the function
#'   will skip cloudy images.
#' @param buffer.clouds distance in meters around the area
#'   of interest within a cloud would be considered to
#'   interfere with the index calculation. This is useful to
#'   eliminate the effect of cloud shading from the
#'   analysis.
#' @param pixel.res pixel resolution used to compute the
#'   vegetation index. Can be one of 10m, 20m, 30m
#' @param img.formats image formats to search for in the
#'   zipped file
#' @param downscale.to the resolution in meters to downscale the 
#'   resolution of the vegetation index raster layer
#' @param fun function to be applied to consolidate duplicated images
#' @param verbose whether to display information on the
#'   progress of operations
#' @details This is script that unzips the Sentinel 2 zipped
#'   file into a temporary folder, searches for the
#'   index-relevant bands, and then computes the index. If
#'   no \sQuote{aoi} is provided, the script will compute
#'   the vegetation index for the area covered by the image.
#'   The pre-specified vegetation indices are computed as follows:
#'   \deqn{BSI = \frac{(SWIR + RED) - (NIR + BLUE)}{(SWIR + RED) + (NIR + BLUE)}}
#'   \deqn{EVI = \frac{2.5 \times (NIR - RED)}{(NIR + (6 \times RED) - (7.5 \times BLUE) - 1) }}
#'   \deqn{GCVI = \frac{(NIR)}{(GREEN)} - 1}
#'   \deqn{NDRE = \frac{(NIR - RED edge)}{(NIR + RED edge)}} 
#'   \deqn{NDVI = \frac{(NIR - RED)}{(NIR + RED)}} 
#'   \deqn{RECI = \frac{(NIR)}{(RED edge)} - 1}
#'   
#'   The user can also specify custom vegetation indices using the formula 
#'   argument. The formula should be two-sided, with the left side naming the
#'   vegetation index and the right side defining the mathematical operations
#'   used to calculate the vegetation index. The bands should be specified
#'   as B01, B02, ..., B12.
#' 
#'   An important detail of this function is that, if there are
#'   duplicated dates, the function will consolidate the data into 
#'   a single raster layer. The default behavior is to average the 
#'   layers that belong to the same date. This can be changed with the 
#'   'fun' argument.
#' @return an object of class veg.index and stars
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \donttest{
#' extd.dir <- system.file("extdata", package = "pacu")
#' ## List of zipped Sentinel files in a directory
#' s2a.files <- list.files(extd.dir, '\\.zip', full.names = TRUE)
#' area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'))
#' 
#' ## computing ndvi
#' ndvi <- pa_compute_vi(satellite.images = s2a.files,
#'                             vi = 'ndvi',
#'                             aoi = area.of.interest,
#'                             check.clouds = TRUE)
#'
#' ## computing ndre
#' ndre <- pa_compute_vi(satellite.images = s2a.files,
#'                             vi = 'ndre',
#'                             aoi = area.of.interest,
#'                             check.clouds = TRUE)
#'
#' ## specifying a differente vegetation index, in this case, the 
#' ## excess green index
#' egi <- pa_compute_vi(satellite.images = s2a.files,
#'                             vi = 'other',
#'                             formula = EGI ~ (2 * B03) - B02 - B04,
#'                             aoi = area.of.interest,
#'                             check.clouds = TRUE)
#' }
#'
pa_compute_vi <- function(satellite.images,
                          vi =c('ndvi', 'ndre', 'gcvi', 'reci', 'evi', 'bsi', 'other'),
                          aoi = NULL,
                          formula = NULL,
                          check.clouds = FALSE,
                          buffer.clouds = 100,
                          downscale.to = NULL,
                          pixel.res = c('default', '10m', '20m', '60m'),
                          img.formats = c('jp2', 'tif'),
                          fun = function(x) mean(x, na.rm = TRUE),
                          verbose = TRUE){
  
  pixel.res <- match.arg(pixel.res)
  vi <- match.arg(vi)
  
  req.namespaces <- c('stars', 'sf')
  for (ns in req.namespaces) {
    if(!requireNamespace(ns, quietly = TRUE)){
      warning('The ', ns, ' package is required for this function')
      return(NULL)
    }
  }
  if(is.null(aoi) && check.clouds == TRUE) 
    stop('When check.clouds is TRUE, aoi must be supplied')
  
  if(length(satellite.images) < 1)
    stop('There are no images in satellite.images')
  
  if (!is.null(formula) && vi != 'other')
    stop('When formula is not NULL, vi must be other')
  
  

  iops <- list(ndvi = expression((B08 - B04) / (B08 + B04)),
               ndre = expression((B08 - B05) / (B08 + B05)),
               gcvi = expression(B08/B03 - 1),
               reci = expression(B08/B05 - 1),
               evi = expression(2.5 * (B08 - B04) / ((B08 + 6.0 * B04 - 7.5 * B02) + 1.0)),
               bsi = expression(((B11 + B04) - (B08 + B02)) / ((B11 + B04) + (B08 + B02))))
  
  
  if (vi == 'other'){
    ibands <- all.vars(formula)[-1]
    vi <- all.vars(formula)[1]
    form <- as.list(formula)
    form <- form[[length(form)]]
    ind.op <- as.expression(form)
  }else{
    ind.op <- iops[[vi]]
    ibands <- all.vars(ind.op)
  }
  
  res <- list()
  
  if(verbose == 1){
    progress.bar <- utils::txtProgressBar(min = 0, 
                                          max = length(satellite.images),
                                          style = 3,
                                          initial = 0)
    on.exit(close(progress.bar))
  }
  
  for (sat.img in satellite.images) {
    if (verbose > 1) {
      cat('processing ', sat.img, '\n')
    }
    
    if(!is.null(aoi) && check.clouds == TRUE) {
      clouds <- .pa_get_cloud_polygon(sat.img)
      
      if(!is.null(clouds)){
        boundary <- sf::st_as_sfc(sf::st_bbox(sf::st_transform(aoi, sf::st_crs(clouds))))
        cloud.buffer <- sf::st_buffer(boundary, buffer.clouds)
        clouds <- sf::st_crop(clouds, cloud.buffer)
        clouds <- as.data.frame(clouds)
        clouds <- stats::na.omit(clouds)
        check.overlap <- FALSE
        if(any(clouds[[3]] > 0)){check.overlap <- TRUE}
        
        
        if(check.overlap) {
          if( verbose == 1){
            warning('Clouds detected over the AOI. Skipping ', basename(sat.img))
            utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1) 
          }
          next
        }
        
      }
    }
    
    bname <- basename(sat.img)
    bname <- strsplit(bname, '\\.')[[1]][1]
    temporary.dir <- tempdir(check = TRUE)
    temporary.dir <- file.path(temporary.dir,'pa_compute_vi', bname)
    
    dir.create(temporary.dir, 
               showWarnings = FALSE,
               recursive = TRUE)
    
    imgList <- utils::unzip(sat.img[[1]], 
                            list = TRUE)
    files.tgt <- .pa_select_s2_files(sat.img[[1]])
    files.out <- sapply(files.tgt, function(x) grep(x, imgList[[1]], value = TRUE))
    
    utils::unzip(sat.img[[1]], 
                 files = files.out,
                 overwrite = TRUE, 
                 exdir = temporary.dir, 
                 junkpaths = TRUE)
    
    rs <- list()
    for (b in ibands){
      bpath <- .pa_get_band(b, temporary.dir, pixel.res, img.formats)
      bimg <- stars::read_stars(bpath)
      
      if(!is.null(downscale.to)){
        crt.crs <- sf::st_crs(bimg)
        is.utm <- grepl('UTM zone', crt.crs$wkt)
        if (!is.utm)
          stop('When downscale.to is not NULL, CRS should be UTM')
        new.bimg <- stars::st_as_stars(sf::st_bbox(bimg), 
                                       dx = downscale.to, 
                                       dy = downscale.to)
        bimg <- stars::st_warp(bimg, new.bimg)
      }
      
      if(!is.null(aoi)){
        #boundary <- sf::st_geometry(sf::st_transform(aoi, sf::st_crs(bimg)))
        aoi <- pa_2utm(aoi)
        boundary <- sf::st_geometry(sf::st_union(aoi))
        bimg <- stars::st_warp(bimg, crs = sf::st_crs(boundary))
        bimg <- sf::st_crop(bimg, boundary, crop = TRUE)
      }
      
      
      rs <- suppressWarnings(append(rs, list(bimg)))
    }
    
    resolutions <- lapply(rs, function(x) stars::st_res(x)[[1]])
    if (length(unique(resolutions)) > 1) {
      i.greater <- which.max(resolutions)
      for(i in (1:length(rs))[-i.greater]){
        rs[[i]] <- stars::st_warp(stars::st_as_stars(rs[[i]]),
                                  stars::st_as_stars(rs[[i.greater]]))
      }
    }
    
    names(rs) <- ibands
    img <- eval(ind.op, rs)
    img <- stars::st_as_stars(img)
    
    metadata.file <- .pa_select_s2_files(sat.img, which = 'metadata')
    metadata.file <- grep(metadata.file, list.files(temporary.dir), value = TRUE)
    metadata <- .pa_read_s2_metadata(file.path(temporary.dir, metadata.file))
    timestamp <- metadata$General_Info$Product_Info$PRODUCT_START_TIME
    timestamp <- as.Date(timestamp)
    img <- stars::st_redimension(img,
                                 new_dims = c(dim(img), 1))
    img <- stars::st_set_dimensions(img, names = c('x', 'y', 'time'))
    img <- stars::st_set_dimensions(img, 3, timestamp)
    names(img) <- vi
    res[[length(res) + 1]] <- img
    
    if( verbose == 1){
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1) 
    }
    
  }
  
  sorted <- sapply(res, function(x) stars::st_get_dimension_values(x, 'time'))
  sorted <- order(sorted)
  res <- res[sorted]
  if (length(res) > 1){
    res <- .pa_align_bbox(res)
    res <- .pa_consolidate_dates(res, fun = fun) 
  }
  res <- do.call(c, c(res, along = 'time'))
  
  # if (!is.null(downscale.to)){
  #   crt.crs <- sf::st_crs(res)
  #   is.utm <- grepl('UTM zone', crt.crs$wkt)
  #   if (!is.utm)
  #     stop('When downscale.to is not NULL, CRS should be UTM')
  #   
  #   new.res <- stars::st_as_stars(sf::st_bbox(res), 
  #                                 dx = downscale.to, 
  #                                 dy = downscale.to)
  #   res <- stars::st_warp(res, new.res)
  # }
  
  attr(res, 'vegetation.index') <- vi
  class(res) <- c('veg.index', class(res))
  
  
  return(res)
}

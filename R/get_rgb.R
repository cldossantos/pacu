#'
#' @title Retrieve an RGB image from a zipped Sentinel 2 file
#' @description Retrieve an RGB image from a zipped Sentinel 2 file
#' @name pa_get_rgb
#' @param satellite.images list of file paths to the Sentinel 2 zip files
#' @param aoi NULL or an sf object used to crop the RGB raster to an area of interest
#' @param pixel.res pixel resolution used to retrieve the RGB image. Can be one of 10m, 20m, 30m.
#' @param check.clouds whether to check for clouds over the
#'   area of interest. If clouds are found, the function
#'   will skip cloudy images.
#' @param buffer.clouds distance in meters around the area
#'   of interest within a cloud would be considered to
#'   interfere with the index calculation. This is useful to
#'   eliminate the effect of cloud shading from the
#'   analysis.
#' @param img.formats image formats to search for in the zipped file
#' @param rgb.bands a vector containing the order of the RGB bands
#' @param fun function to be applied to consolidate duplicated images
#' @param verbose whether to display information on the
#'   progress of operations
#' @details This is script that unzips the Sentinel 2 zipped file into a temporary folder, searches for the RGB,
#' and constructs a multi-band raster containing the RGB bands.
#' If no \sQuote{aoi} is provided, the script will construct the RGB image for the area covered by the image.
#'   An important detail of this function is that, if there are
#'   duplicated dates, the function will consolidate the data into 
#'   a single raster layer. The default behavior is to average the 
#'   layers that belong to the same date. This can be changed with the 
#'   'fun' argument.
#' @return an object of class rgb and stars
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \donttest{
#' extd.dir <- system.file("extdata", package = "pacu")
#' ## List of zipped Sentinel files in a directory
#' s2a.files <- list.files(extd.dir, '\\.zip', full.names = TRUE)
#' area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'))
#' rgb.rast <- pa_get_rgb(satellite.images = s2a.files,
#'                             aoi = area.of.interest)
#' pa_plot(rgb.rast)
#' }
#'
pa_get_rgb <- function(satellite.images,
                        aoi = NULL,
                        pixel.res = '10m',
                          check.clouds = FALSE,
                          buffer.clouds = 100,
                        img.formats = c('jp2', 'tif'),
                       rgb.bands = c('B04', 'B02', 'B03'),
                       fun = function(x) mean(x, na.rm = TRUE), 
                       verbose = TRUE){

  
  extensions <- paste0(img.formats, collapse = '|')
  extensions <- paste0('\\.(', extensions, ')$')
  res <- list()

  req.namespaces <- c('stars', 'sf')
  for (ns in req.namespaces) {
    if(!requireNamespace(ns, quietly = TRUE)){
      warning('The ', ns, ' package is required for this function')
      return(NULL)
    }
  }
  
  if(length(satellite.images) < 1)
    stop('There are no images in satellite.images')

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
    
    ## Get list of files inside of the zip file
    bname <- basename(sat.img)
    bname <- strsplit(bname, '\\.')[[1]][1]
    temporary.dir <- tempdir(check = TRUE)
    
    temporary.dir <- file.path(temporary.dir,'pa_get_rgb', bname)
    
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
    for  (b in rgb.bands){
      bpath <- .pa_get_band(b, temporary.dir, pixel.res, img.formats)
      bimg <- stars::read_stars(bpath)

      if(!is.null(aoi)){
        aoi <- pa_2utm(aoi)
        boundary <- sf::st_geometry(sf::st_union(aoi))
        bimg <- stars::st_warp(bimg, crs = sf::st_crs(boundary))
        bimg <- sf::st_crop(bimg, boundary, crop = TRUE)
      }
      rs[[length(rs) + 1]] <- bimg
    }

    img <- do.call(c, rs)
    names(img) <- c('R', 'G', 'B')
    
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
    res[[length(res) + 1]] <- img
    class(img) <- c('rgb', class(img))

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
  class(res) <- c('rgb', class(res))
  return(res)
}

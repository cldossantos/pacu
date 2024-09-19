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
#' @param verbose whether to display information on the
#'   progress of operations
#' @details This is script that unzips the Sentinel 2 zipped
#'   file into a temporary folder, searches for the
#'   index-relevant bands, and then computes the index. If
#'   no \sQuote{aoi} is provided, the script will compute
#'   the vegetation index for the area covered by the image.
#'   The vegetation indices are computed as follows:
#'   \deqn{BSI = \frac{(SWIR + RED) - (NIR + BLUE)}{(SWIR + RED) + (NIR + BLUE)}}
#'   \deqn{EVI = \frac{2.5 \times (NIR - RED)}{(NIR + (6 \times RED) - (7.5 \times BLUE) - 1) }}
#'   \deqn{GCVI = \frac{(NIR)}{(GREEN)} - 1}
#'   \deqn{NDRE = \frac{(NIR - RED edge)}{(NIR + RED edge)}} 
#'   \deqn{NDVI = \frac{(NIR - RED)}{(NIR + RED)}} 
#'   \deqn{RECI = \frac{(NIR)}{(RED edge)} - 1}
#' 
#' @return an object of class veg.index and stars
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "pacu")
#' ## List of zipped Sentinel files in a directory
#' s2a.files <- list.files(extd.dir, '\\.zip', full.names = TRUE)
#' area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'))
#' ndvi <- pa_compute_vi(satellite.images = s2a.files,
#'                             vi = 'ndvi',
#'                             aoi = area.of.interest,
#'                             check.clouds = TRUE)
#'
#' ndre <- pa_compute_vi(satellite.images = s2a.files,
#'                             vi = 'ndre',
#'                             aoi = area.of.interest,
#'                             check.clouds = TRUE)
#'
#'
#' }
#'
pa_compute_vi <- function(satellite.images,
                          vi =c('ndvi', 'ndre', 'gcvi', 'reci', 'evi', 'bsi'),
                          aoi = NULL,
                          check.clouds = FALSE,
                          buffer.clouds = 100,
                          pixel.res = c('default', '10m', '20m', '60m'),
                          img.formats = c('jp2', 'tif'),
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
  
  

  ibands <- list(ndvi = c('B08', 'B04'),
                 ndre = c('B08', 'B05'),
                 gcvi = c('B08', 'B03'),
                 reci = c('B08', 'B05'),
                 evi = c('B08', 'B04', 'B02'),
                 bsi = c('B11', 'B04', 'B02', 'B08' ))

  iops <- list(ndvi = expression((b[[1]] - b[[2]]) / (b[[1]] + b[[2]])),
               ndre = expression((b[[1]] - b[[2]]) / (b[[1]] + b[[2]])),
               gcvi = expression(b[[1]]/b[[2]] - 1),
               reci = expression(b[[1]]/b[[2]] - 1),
               evi = expression(2.5 * (b[[1]] - b[[2]]) / ((b[[1]] + 6.0 * b[[2]] - 7.5 * b[[3]]) + 1.0)),
               bsi = expression(((b[[1]] + b[[2]]) - (b[[4]] + b[[3]])) / ((b[[1]] + b[[2]]) + (b[[4]] + b[[3]]))))


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
        check.overlap <- sf::st_intersects(cloud.buffer, clouds, sparse = FALSE)

        if(any(check.overlap)) {
          warning('Clouds detected over the AOI. Skipping to the next image.')
          next
        }

      }
    }

    bname <- basename(sat.img)
    bname <- strsplit(bname, '\\.')[[1]][1]
    temporary.dir <- tempdir(check = TRUE)
    temporary.dir <- file.path(temporary.dir, bname)
    dir.create(temporary.dir, showWarnings = FALSE, recursive = TRUE)
    utils::unzip(sat.img[[1]], overwrite = TRUE, exdir = temporary.dir, junkpaths = TRUE)

    rs <- list()
    for (b in ibands[[vi]]){
      bpath <- .pa_get_band(b, temporary.dir, pixel.res, img.formats)
      bimg <- stars::read_stars(bpath)

      if(!is.null(aoi)){
        boundary <- sf::st_geometry(sf::st_transform(aoi, sf::st_crs(bimg)))
        bimg <- sf::st_crop(bimg, boundary, crop = TRUE)
      }

      rs <- suppressWarnings(append(rs, list(bimg)))
    }

    resolutions <- lapply(rs, function(x) stars::st_res(x)[[1]])
    if (length(unique(resolutions)) > 1) {
      i.greater <- which.max(resolutions)
      for(i in (1:length(rs))[-i.greater]){
        rs[[i]] <- stars::st_warp(rs[[i]], rs[[i.greater]])
      }
    }

    op <- iops[[vi]]
    img <- eval(op, list(b = rs))

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

  res <- do.call(c, c(res, along = 'time'))
  attr(res, 'vegetation.index') <- vi
  class(res) <- c('veg.index', class(res))
  return(res)
}

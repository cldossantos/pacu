#'
#' @title Retrieve an RGB image from a zipped Sentinel 2 file
#' @description Retrieve an RGB image from a zipped Sentinel 2 file
#' @name pa_get_rgb
#' @param satellite.images list of file paths to the Sentinel 2 zip files
#' @param aoi NULL or an sf object used to crop the RGB raster to an area of interest
#' @param pixel.res pixel resolution used to retrieve the RGB image (10m, 20m, 30m)
#' @param img.formats image formats to search for in the zipped file
#' @param rgb.bands a vector containing the order of the RGB bands
#' @details This is script that unzips the Sentinel 2 zipped file into a temporary folder, searches for the RGB,
#' and constructs a multi-band raster containing the RGB bands.
#' If no \sQuote{aoi} is provided, the script will construct the RGB image for the area covered by the image.
#' @return an object of class rgb and stars
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \dontrun{
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
                        img.formats = c('jp2', 'tif'),
                       rgb.bands = c('B04', 'B02', 'B03')){

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

  for (sat.img in satellite.images) {

    ## Get list of files inside of the zip file
    imgList <- utils::unzip(sat.img[[1]], list = TRUE)

    ## Match the image names
    image.indices <- grep(paste0('IMG_DATA/.{1,}B[0-9]{2,2}.{1,}', extensions), imgList[[1]], value = TRUE)
    temporary.dir <- tempdir(check = TRUE)
    utils::unzip(sat.img[[1]], overwrite = TRUE, exdir = temporary.dir, junkpaths = TRUE)
    rs <- list()
    for  (b in rgb.bands){
      bpath <- .pa_get_band(b, temporary.dir, pixel.res, img.formats)
      bimg <- stars::read_stars(bpath)

      if(!is.null(aoi)){
        boundary <- sf::st_geometry(sf::st_transform(aoi, sf::st_crs(bimg)))
        bimg <- sf::st_crop(bimg, boundary, crop = TRUE)
      }
      rs[[length(rs) + 1]] <- bimg
    }

    img <- do.call(c, rs)
    names(img) <- c('R', 'G', 'B')

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

  }
  sorted <- sapply(res, function(x) stars::st_get_dimension_values(x, 'time'))
  sorted <- order(sorted)
  res <- res[sorted]
  res <- do.call(c, c(res, along = 'time'))
  class(res) <- c('rgb', class(res))
  return(res)
}

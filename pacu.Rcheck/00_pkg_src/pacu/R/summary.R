#' @title Produce result summaries of the various pacu objects
#' @description  Produce summaries for the different pacu objects
#' @name summary
#' @rdname pa_summary
#' @param object object to be summarized
#' @param ... additional arguments. None used currently.
#' @exportS3Method base::summary
#' @return when object is of class dslist, no return value. Called for side effects.
#' @export
summary.dslist <- function(object, ...){

  x <- object
  x$Year <- as.numeric(strftime(as.POSIXct(x$ContentDate[[1]]), '%Y'))
  x$Month <- strftime(as.POSIXct(x$ContentDate[[1]]), '%m')

  out <- stats::aggregate(list(Count = x[, 1]),
                          x[c('Year', 'Month')],
                          length)

  .pa_print_table(out)

  .pa_print_table(data.frame(a = 'Total', b = sum(out$Count)), headers = FALSE,
                  frame = FALSE, sep = '\t')

  return(invisible(out))
}



#' @param by column names used to group the summary. Defaults to NULL, resulting in
#' an overall summary for the field
#' @rdname pa_summary
#' @exportS3Method base::summary
#' @return when object is of class yield, returns an object of class data.frame
#' @export
summary.yield <- function(object,
                          ...,
                          by = NULL){

  x <- as.data.frame(object$yield)

  if(!all(by %in% names(x)))
    stop('Argument "by" must be NULL or a valid column in the data set.')

  summ.fn <- function(y) c(mean = mean(y, na.rm = TRUE), sd = stats::sd(y, na.rm = TRUE))

  if (is.null(by)){
    out <- stats::aggregate(x[[1]],
                            list(rep('field', length(x[[1]]))),
                            summ.fn)
    out <- do.call(data.frame, out)
    names(out) <- c('Group', 'yield.mean', 'yield.sd')

  }else{
    out <- stats::aggregate(x[[1]],
                            x[by],
                            summ.fn)
    out <- do.call(data.frame, out)
    names(out) <- c(by, 'yield.mean', 'yield.sd')
  }
  return(out)

}






#' @param by sf or stars object containing the geometries
#'   within which the vegetation index values should be
#'   summarized
#' @param fun a function to be applied when summarizing the
#'   vegetation index data. For example, mean, median, max,
#'   min.
#' @rdname pa_summary
#' @return when object is of class veg.index, returns an object of class stars
#' @exportS3Method base::summary
#' @export
summary.veg.index <- function(object,
                              ...,
                              by,
                              fun){

  vegetation.index <- attr(object, 'vegetation.index')
  crs.x <- sf::st_crs(object)
  crs.y <- sf::st_crs(by)
  
  if(!(crs.x == crs.y)){
    warning('CRS of "by" is different from the CRS of "object".',
            'Reprojecting "by".')
    by <- sf::st_transform(by, crs.x)
  }
  
  bbox.x <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(object)))
  bbox.y <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(by)))
  overlap <- as.logical(unlist(sf::st_intersects(bbox.x, bbox.y)))
  
  if(!overlap){
    stop('object and by do not overlap.')
  }
  
  object <- sf::st_crop(object, bbox.y)
  x <-  stats::aggregate(x = object, by = by, FUN = fun, ...)
  x <-  sf::st_join(x, by, join = sf::st_equals, left = FALSE)
  class(x) <- c('veg.index', class(x))

  attr(x, 'vegetation.index') <- vegetation.index

  return(x)

}



#' @title Produce result summaries of the various pacu objects
#' @description  Produce summaries for the different pacu objects
#' @name summary
#' @rdname pa_summary
#' @param object object to be summarized
#' @param ... additional arguments. None used currently.
#' @exportS3Method base::summary
#' @export
summary.dslist <- function(object, ...){

  x <- object
  x$Year <- as.numeric(strftime(as.POSIXct(x$OriginDate), '%Y'))
  x$Month <- strftime(as.POSIXct(x$OriginDate), '%m')

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
#' @exportS3Method base::summary
#' @export
summary.veg.index <- function(object,
                              ...,
                              by,
                              fun){

  x <-  stats::aggregate(x = object, by = by, FUN = fun, ...)
  x <-  sf::st_join(x, by, join = sf::st_equals, left = FALSE)
  class(x) <- c('veg.index', class(x))

  attr(x, 'vegetation.index') <- attr(object, 'vegetation.index')

  return(x)

}



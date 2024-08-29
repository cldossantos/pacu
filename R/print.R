
#' @title Print a pacu object
#' @description  These functions print meaningful information from
#' pacu objects.
#' @name print
#' @rdname pa_print
#' @param x object to be printed
#' @param ... additional arguments. None used currently.
#' @export
print.yield <- function(x, ...){

  cat('Yield data processing algorithm:', attr(x$yield, 'algorithm'), '\n')
  cat('Smoothing method:', attr(x$yield, 'smooth.method'), '\n')

  if (!is.null(attr(x$yield, 'lbs.per.bushel')))
    cat('Conversion factor: ', attr(x$yield, 'lbs.per.bushel'), 'lbs/bushel\n')
  cat('Adj. moisture: ', attr(x$yield, 'moisture'), '%', '\n', sep = '')
  cat('Yield  summary (', attr(x$yield, 'units'), ')', '\n', sep = '')
  #print(summary(x$yield[[1]]))
  summ <- summary(x$yield[[1]])
  summ <- data.frame(statistic = names(summ), value = as.numeric(summ))
  summ$value <- round(summ$value, getOption('digits'))
  .pa_print_table(summ, headers = FALSE)

  if (!is.null(x$variogram)){
    model.table <- gstat::vgm()
    model.table$long <- as.character(model.table$long)
    cat('Variogram model:', model.table[match(x$variogram.model$model, model.table[, 1]), 2], '\n')
    cat('Partial sill: ', x$variogram.model$psill, '\n')
    cat('Range: ', x$variogram.model$range, '\n')
    if(!is.null(x$variogram.model$kappa))
      cat('Kappa: ', x$variogram.model$kappa, '\n')
  }


}


#' @rdname pa_print
#' @export
print.dslist <- function(x, ...){

  cat('Search parameters\n')
  dates <- c(attr(x, 'start.date'), attr(x, 'end.date'))
  dates <- as.Date(dates)
  dates <- strftime(dates, '%Y-%m-%d')
  cat('Start date:', dates[1], '\n')
  cat('End date:', dates[2], '\n')
  cat('Max. cloud cover: ', attr(x, 'max.cloud.cover'), '%', '\n', sep = '')
  cat('Collection name: ', attr(x, 'collection.name'), '\n')
  cat('\nResults\n')
  cat('Total: ', dim(x)[1], '\n')
  cat('Online: ', sum(x$Online), '\n')



  }


#' @title Print a pacu object
#' @description  These functions print meaningful information from
#' pacu objects.
#' @name print
#' @rdname pa_print
#' @param x object to be printed
#' @param ... additional arguments. None used currently.
#' @return No return value, called for side effects
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


#' @rdname pa_print
#' @export
print.check.yield <- function(x, ...){
  column <-  NULL
  
  cat('Field information\n')
  .pa_print_table(x$field.info,
                  headers = FALSE,
                  width = c(NA, 5))
  if (is.na(x$field.info$values[match('CRS',x$field.info$category)]))
    message('The CRS is missing from the input object. The pa_yield function will default to EPSG:4326\n')
  
  ## simple
  if (!any(is.na(x$check.simple))){
    
    cat('\n\nAlgorithm: Simple', fill = TRUE)
    cat('Checking column names and units\n', sep = '')
    .pa_print_table(x$check.simple$data)
    cat('Checking data values\n')
    .pa_print_table(x$check.simple$values)
    
    if (any(x$check.simple$values$extreme > 0))
      warning('Extreme values identified. These these are values outside of the range of the mean \u00B1 3sd',
              immediate. = TRUE)
    
    not.found <- subset(x$check.simple$data, column == '-')[['variable']]
    
    
    if (any(c('yield', 'moisture') %in% not.found))
      warning('Columns yield and moisture are needed for the simple algorithm',
              immediate. = TRUE)
    
    if ('interval' %in% not.found)
      warning('Column interval is needed for the simple algorithm',
              ' when using the lag.adjust argument',
              immediate. = TRUE)
  }
    
 ## ritas 
  if (!any(is.na(x$check.ritas))){
    
    cat('\n\nAlgorithm: RITAS', fill = TRUE)
    cat('Checking column names and units\n', sep = '')
    .pa_print_table(x$check.ritas$data)
    cat('Checking data values\n')
    .pa_print_table(x$check.ritas$values)
    
    not.found <- subset(x$check.ritas$data, column == '-')[['variable']]
    
    if (any(x$check.ritas$values$extreme > 0))
      warning('Extreme values identified. These these are values outside of the range of the mean \u00B1 3sd',
              immediate. = TRUE)


    not.found <- subset(x$check.ritas$data, column == '-')[['variable']]


    if (('mass' %in% not.found && any(c('interval', 'flow') %in% not.found)))
      warning('Either mass or, simutaneously, interval and flow are required for the ritas algorithm',
              immediate. = TRUE)

    if(!('mass' %in% not.found) & 'interval' %in% not.found )
      message('When mass is in the data set, interval is only required during the ritas algorithm if',
              ' lag.adjust > 0')


    if ('angle' %in% not.found)
      warning('Column angle not found but can be estimated within pa_yield',
              immediate. = TRUE)
    
    if (is.na(x$check.ritas$overlap)){
      message('Unable to check polygon overlap due to missing information')
    }else{
      cat('Median overlap between harvest polygons is',
          x$check.ritas$overlap, '%',
          '\nIf this value seems high, please check the units of distance and swath.\n')
    }
    
  }
}


#' @rdname pa_print
#' @export
print.trial <- function(x, ...){



  first <- data.frame('Variable' = attr(x$trial, 'resp'), 
             'Algorithm' = attr(x$trial, 'algorithm'),
             'Smoothing' = attr(x$trial, 'smooth.method'),
             'Units' = attr(x$trial, 'units'))
  
  cat('Variables in trial object:\n')
  .pa_print_table(first, headers = TRUE)
  xd <- as.data.frame(x$trial)
  xd <- xd[attr(x$trial, 'resp')]
  summaries <- vector('list', length(attr(x$trial, 'resp')))
  
  cat('\nVariable summary:\n')
  for ( i in 1:length(summaries)){
    summ <- stats::fivenum(xd[[i]])
    nas <- sum(is.na(xd[[i]]))
    xbar <- mean(xd[[i]], na.rm = TRUE)
    summ <- c(summ, xbar, nas)
    summ <- data.frame(value = summ)
    summ$value <- round(summ$value, getOption('digits')) 
    summaries[[i]] <- summ
  }
  
  summ <- do.call(cbind, summaries)
  ids <- data.frame(statistic = c('Min.', '1st Qt.', 'Median', '3rd Qt.', 'Max.', 'Mean', 'NAs'))
  summ <- cbind(ids, summ)
  names(summ) <- c(' ', attr(x$trial, 'resp'))
  
  .pa_print_table(summ, headers = TRUE)


}
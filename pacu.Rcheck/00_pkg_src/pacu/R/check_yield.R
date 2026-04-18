#'
#' @title Check yield data before processing with pa_yield
#' @description Check for red flags in raw yield monitor data before
#'   processing with `pa_yield()`.
#' @name pa_check_yield
#' @rdname pa_check_yield
#' @param input an sf object containing the input data from
#'   a yield monitor
#' @param algorithm algorithm for which the function should
#'   check the data. Different algorithms require different
#'   information in the input data set.
#' @details This function checks the input yield data for potential problems
#'   before running `pa_yield()`.
#' @return object of class check.yield
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \donttest{
#' extd.dir <- system.file("extdata", package = "pacu")
#' raw.yield <- sf::read_sf(file.path(extd.dir, '2012-basswood.shp'),
#'                          quiet = TRUE)
#' chk <- pa_check_yield(input = raw.yield)
#' chk
#' }
#'
pa_check_yield <- function(input,
                           algorithm = c('all', 'simple', 'ritas')){

  column <- NULL

  s.wrns <-  get("suppress.warnings", envir = pacu.options)
  s.msgs <-  get("suppress.messages", envir = pacu.options)
  res <- list()
  
  algorithm <- match.arg(algorithm)
   ## checking for columns and units
  checks <- list(simple = c('yield', 'moisture', 'interval'),
                 ritas =  c('mass', 'flow', 'moisture', 'interval', 'angle', 'width', 'distance'))

  if (algorithm != 'all')
    checks <- checks[algorithm]

  crt.crs <- sf::st_crs(input)
  if(is.na(crt.crs)) {
    sf::st_crs(input) <- 'epsg:4326'
  }

  center <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_union(input))))
  lat <- center[2]
  lon <- center[1]
  area <- sf::st_area(.pa_field_boundary(sf::st_geometry(input)))
  area <- as.numeric(area)


  res[[1]] <- data.frame(category = c('# of points', 'Approx. area (ha)', 'Approx. area (ac)', 'Latitude', 'Longitude', 'CRS'),
                                              values = c(nrow(input), area / 1e4, area / 4046, lat, lon, crt.crs$input))

  names(res)[1] <- 'field.info'
  
  if (sf::st_is_longlat(input)){
    if(lon < -180 || lon > 180) warning("longitude should be between -180 and 180")
    if(lat < -90 || lat > 90) warning("latitude should be between -90 and 90")
  }

  message.list <- list()

  res[[2]] <- NA
  names(res)[2] <- c('check.simple')
  
  if ('simple' %in% names(checks)){

    alg <-  'simple'


    length.ans <- length(checks[[alg]])
    ans <- data.frame(variable = rep(NA, length.ans),
                      column = rep(NA, length.ans),
                      mean = rep(NA, length.ans),
                      units = rep(NA, length.ans))


    ans2 <- data.frame(variable = rep(NA, length.ans),
                       min = rep(NA, length.ans),
                       max = rep(NA, length.ans),
                       median = rep(NA, length.ans),
                       NAs = rep(NA, length.ans),
                       extreme = rep(NA, length.ans))

    for (i in 1:length.ans){
      var <- checks[[alg]][i]
      tgt.col <- .pa_get_variable_names(var)
      tgt.index <- which(tolower(names(input)) %in% tgt.col)

      if (var == 'interval' && length(tgt.index) < 1) {
        time.col <- .pa_get_variable_columns(input, 'time', TRUE)
        if (!is.null(time.col)){
          time <- input[[time.col]]
          interval <- try(.pa_time2interval(time), silent = TRUE)
          if (!inherits(interval, 'try-error')){
            interval <- .pa_enforce_units(interval, 'time')
            input$interval <- interval
            tgt.index <- dim(input)[2]
            if (!s.wrns)
              warning('The variable interval was not found, thus, it was estimated from the col ', time.col)
          }else{
            if (!s.wrns)
              warning('The variable interval was not found and the function could not estimate it from ', time.col)
          }
        }

      }

      if(length(tgt.index) < 1) {
        warnings.list <- paste('Could not find column for', var)
        ans[i, 'variable'] <- var
        ans[i, -1] <- '-'

        ans2[i, 'variable'] <- var
        ans2[i, -1] <- '-'

      }else{
        variable.column <- names(input)[tgt.index]
        variable.data <- input[[variable.column]]
        variable.units <-  suppressMessages(.pa_guess_units(variable.data, var, verbose = FALSE))
        ul <- units(variable.units)
        ul <- paste(ul$numerator, ul$denominator,
                    sep = ifelse(length(ul$denominator) > 0, '/', ''))

        ans[i, 'variable'] <- var
        ans[i, 'column'] <- variable.column
        ans[i, 'mean'] <- round(mean(variable.data, na.rm = TRUE), 1)
        ans[i, 'units'] <- ul


        ans2[i, 'variable'] <- var
        ans2[i, 'min'] <- min(variable.data, na.rm = TRUE)
        ans2[i, 'max'] <- max(variable.data, na.rm = TRUE)
        ans2[i, 'median'] <- stats::median(variable.data, na.rm = TRUE)
        ans2[i, 'NAs'] <- sum(is.na(variable.data))
        pq <- 3 * stats::sd(variable.data, na.rm = TRUE)
        extreme.values <- variable.data[variable.data < mean(variable.data, na.rm = TRUE) - pq |
                                          variable.data > mean(variable.data, na.rm = TRUE) + pq]
        ans2[i, 'extreme'] <- length(extreme.values)
      }

      ans$units <- gsub('bushel/acre', 'bu/ac', ans$units)
    }
    
    res[[2]] <- list(ans, ans2)
    names(res[[2]]) <- c('data', 'values')



    
  }

  res[[3]] <- NA
  names(res)[3] <- 'check.ritas'
  
  if ('ritas' %in% names(checks)){

    alg <- 'ritas'

    length.ans <- length(checks[[alg]])
    ans <- data.frame(variable = rep(NA, length.ans),
                      column = rep(NA, length.ans),
                      mean = rep(NA, length.ans),
                      units = rep(NA, length.ans))

    ans2 <- data.frame(variable = rep(NA, length.ans),
                       min = rep(NA, length.ans),
                       max = rep(NA, length.ans),
                       median = rep(NA, length.ans),
                       NAs = rep(NA, length.ans),
                       extreme = rep(NA, length.ans))

    for (i in 1:length.ans){
      var <- checks[[alg]][i]
      tgt.col <- .pa_get_variable_columns(input, var, verbose = FALSE)

      if (var == 'interval' && is.null(tgt.col)) {
        time.col <- .pa_get_variable_columns(input, 'time', verbose = FALSE)
        if (!is.null(time.col)){
          time <- input[[time.col]]
          interval <- try(.pa_time2interval(time), silent = TRUE)
          if (!inherits(interval, 'try-error')){
            interval <- .pa_enforce_units(interval, 'time')
            input$interval <- interval
            tgt.index <- dim(input)[2]
            if (!s.wrns)
              warning('The variable interval was not found, thus, it was estimated from the col ', time.col)
          }else{
            if (!s.wrns)
              warning('The variable interval was not found and the function could not estimate it from ', time.col)
          }
        }
        
      }

      if(is.null(tgt.col)) {
        ans[i, 'variable'] <- var
        ans[i, -1] <- '-'

        ans2[i, 'variable'] <- var
        ans2[i, -1] <- '-'
      }else{
        variable.column <- tgt.col
        variable.data <- input[[variable.column]]
        comparison.vector <- .pa_get_comparison_vector(input, var)
        variable.units <-  suppressMessages(.pa_guess_units(variable.data, var, comparison.vector, verbose = FALSE))
        ul <- units(variable.units)
        ul <- paste(ul$numerator, ul$denominator,
                    sep = ifelse(length(ul$denominator) > 0, '/', ''))

        ans[i, 'variable'] <- var
        ans[i, 'column'] <- variable.column
        ans[i, 'mean'] <- round(mean(variable.data, na.rm = TRUE), 1)
        ans[i, 'units'] <- ul

        ans2[i, 'variable'] <- var
        ans2[i, 'min'] <- min(variable.data, na.rm = TRUE)
        ans2[i, 'max'] <- max(variable.data, na.rm = TRUE)
        ans2[i, 'median'] <- stats::median(variable.data, na.rm = TRUE)
        ans2[i, 'NAs'] <- sum(is.na(variable.data))
        pq <- 3 * stats::sd(variable.data, na.rm = TRUE)
        extreme.values <- variable.data[variable.data < mean(variable.data, na.rm = TRUE) - pq |
                                          variable.data > mean(variable.data, na.rm = TRUE) + pq]
        ans2[i, 'extreme'] <- length(extreme.values)
      }
      ans$units <- gsub('bushel/acre', 'bu/ac', ans$units)
    }

    res[[3]] <- list(ans, ans2, NA)
    names(res[[3]]) <- c('data', 'values', 'overlap')
    
    median.overlap <- try(.pa_check_polygon_overlap(input), silent = TRUE)
    if (!inherits(median.overlap, 'try-error')){
      median.overlap <- 100 * (1 - median.overlap)
      median.overlap <- round(median.overlap, 1)
      res$check.ritas$overlap <- median.overlap
    }
  }
  
  class(res) <- 'check.yield'
  invisible(res)
}




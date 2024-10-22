#'
#' @title Check the yield data before processing with the
#'   pa_yield function
#' @description  This function will check for red flags so
#'   the user can know of potential problems before using
#'   the pa_yield functions
#' @name pa_check_yield
#' @rdname pa_check_yield
#' @param input an sf object containing the input data from
#'   a yield monitor
#' @param algorithm for which algorithm should the function
#'   check the data. Different algorithms require different
#'   information to be present in the input data set.
#' @details This function will check the input yield data
#'   for any potential problems before the user runs the
#'   `pa_yield()` function. Ideally, this function warn the user
#'   of potential problems.
#' @return NULL
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "pacu")
#' raw.yield <- sf::read_sf(file.path(extd.dir, '2012-basswood.shp'),
#'                          quiet = TRUE)
#' pa_check_yield(raw.yield)
#' }
#'
pa_check_yield <- function(input,
                           algorithm = c('all', 'simple', 'ritas')){

  column <- NULL

 algorithm <- match.arg(algorithm)
   ## checking for columns and units
  checks <- list(simple = c('yield', 'moisture', 'interval'),
                 ritas =  c('mass', 'flow', 'moisture', 'interval', 'angle', 'width', 'distance'))

  if (algorithm != 'all')
    checks <- checks[algorithm]

  crt.crs <- sf::st_crs(input)
  if(is.na(crt.crs)) {
    message('The CRS is missing from the input object. The pa_yield function will default to EPSG:4326\n')
    sf::st_crs(input) <- 'epsg:4326'
  }


  center <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_union(input))))
  lat <- center[2]
  lon <- center[1]
  area <- sf::st_area(.pa_field_boundary(sf::st_geometry(input)))
  area <- as.numeric(area)

  cat('Field information\n')
  .pa_print_table(data.frame(category = c('# of points', 'Approx. area (ha)', 'Approx. area (ac)', 'Latitude', 'Longitude', 'CRS'),
                             values = c(nrow(input), area / 1e4, area / 4046, lat, lon, crt.crs$input)),
                  headers = FALSE,
                  width = c(NA, 5))



  if (sf::st_is_longlat(input)){
    if(lon < -180 || lon > 180) warning("longitude should be between -180 and 180")
    if(lat < -90 || lat > 90) warning("latitude should be between -90 and 90")
  }





  message.list <- list()




  if ('simple' %in% names(checks)){

    alg <-  'simple'

    cat('Algorithm:', alg, fill = TRUE)
    cat('Checking column names and units\n', sep = '')

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
        interval <- .pa_time2interval(time)
        interval <- .pa_enforce_units(interval, 'time')
        input$interval <- interval
        tgt.index <- dim(input)[2]
        warning('The variable interval was not found, thus, it was estimated from the time variable.',
                ' When algorithm is simple, this is only needed to adjust for lag.')
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
    .pa_print_table(ans)


    cat('Checking data values\n')
    .pa_print_table(ans2)

    if (any(ans2$extreme > 0))
      warning('Extreme values identified. These these are values outside of the range of the mean \u00B1 3sd',
              immediate. = TRUE)

    not.found <- subset(ans, column == '-')[['variable']]


    if (any(c('yield', 'moisture') %in% not.found))
      warning('Columns yield and moisture are needed for the simple algorithm',
              immediate. = TRUE)

    if ('interval' %in% not.found)
      warning('Column interval is needed for the simple algorithm',
              ' when using the lag.adjust argument',
              immediate. = TRUE)
  }



  if ('ritas' %in% names(checks)){

    alg <- 'ritas'
    cat('Algorithm:', alg, fill = TRUE)
    cat('Checking column names and units\n', sep = '')

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
      tgt.col <- .pa_get_variable_columns(input, var, verbose = TRUE)

      if (var == 'interval' && is.null(tgt.col)) {
        time.col <- .pa_get_variable_columns(input, 'time', TRUE)
        if (!is.null(time.col)){
        time <- input[[time.col]]
        interval <- .pa_time2interval(time)
        interval <- .pa_enforce_units(interval, 'time')
        input$interval <- interval
        tgt.index <- dim(input)[2]
        warning('The variable interval was not found, thus, it was estimated from the time variable.')
        }

      }

      if(is.null(tgt.col)) {
        warnings.list <- paste('Could not find column for', var)
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
    .pa_print_table(ans)


    cat('Checking data values\n')
    .pa_print_table(ans2)
    if (any(ans2$extreme > 0))
      warning('Extreme values identified. These these are values outside of the range of the mean \u00B1 3sd',
              immediate. = TRUE)


    not.found <- subset(ans, column == '-')[['variable']]


    if (('mass' %in% not.found && any(c('interval', 'flow') %in% not.found)))
      warning('Either mass or, simutaneously, interval and flow are required for the ritas algorithm',
              immediate. = TRUE)

    if(!('mass' %in% not.found) & 'interval' %in% not.found )
      message('When mass is in the data set, interval is only required during the ritas algorithm if',
              ' lag.adjust > 0')


    if ('angle' %in% not.found)
      warning('Column angle not found but can be estimated within pa_yield',
              immediate. = TRUE)

    median.overlap <- try(.pa_check_polygon_overlap(input), silent = TRUE)
    if (inherits(median.overlap, 'try-error'))
      message('Unable to check polygon overlap due to missing information')

    if (!inherits(median.overlap, 'try-error')){
      median.overlap <- 100 * (1 - median.overlap)
      median.overlap <- round(median.overlap, 1)
      cat('Median overlap between harvest polygons is',
          median.overlap, '%',
          '\nIf this value seems high, please check the units of distance and swath.\n')
    }
  }



}




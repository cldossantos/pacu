#'
#' @title Predict cardinal dates from satellite image data
#' @name pa_cardinal_dates
#' @rdname pa_cardinal_dates
#' @param x object of class vector or veg.index
#' @return when x is a vector, returns a vector of length 3 with the predicted
#' cardinal dates. When x is a veg.index object, returns a stars object with 
#' spatially distributed cardinal dates
#' @export

pa_cardinal_dates <- function(x, ...) {
  UseMethod('pa_cardinal_dates', x)
}

#' @param x vector containing the date or day of the year of that
#' the satellite data was collected
#' @param y vector containing the satellite data value
#' @param baseline.months vector containing the months used as a baseline reference for 
#' when there are no crops in the field. For example, c(1:3, 12) represent Jan, Feb, Mar, and Dec.
#' @param model a string naming the model to be used to estimate cardinal dates
#' @param prior.means a vector of length three containing the prior means for cardinal dates
#' @export
#' @param prior.vars a vector of length three containing the prior variances for cardinal dates
#' @param bias.correction a vector of length three containing the bias correction factor for cardinal dates
#' @rdname  pa_cardinal_dates
#' 
#' @examples
#' \dontrun{
#' x <- seq(1, 365, 6)
#' y <- nlraa::scard3(x, 120, 210, 300)
#' pa_cardinal_dates.vector(
#'   x = x,
#'   y = y,
#'   model = 'scard3',
#'   prior.means = c(130, 190, 297),
#'   prior.vars = c(11, 13, 18),
#'   bias.correction = c(10, 10, 10)
#' )
#' }
#' 
pa_cardinal_dates.numeric <- function(x,
                                      y,
                                      baseline.months = c(1:3, 12),
                                      model = c('none', "card3", "scard3", "agauss", "harmonic"),
                                      prior.means,
                                      prior.vars,
                                      bias.correction,
                                      ...) {
  
  
  model <- match.arg(model)
  
  if (length(x) != length(y))
    stop('Length of x and y must be the same')
  
  if (model == 'none')
    stop('Please choose a model')
  
  
  if (!inherits(x, c("integer", "numeric", "Date"))) {
    stop("x must be of class numeric or Date")
  }
  
  
  
  if (any(sapply(list(prior.means, prior.vars, bias.correction), length) != 3)) {
    stop("prior.means, prior.vars, and bias.correction should be of length 3")
  }
  
  
  req.namespaces <- c("minpack.lm", "nlraa", "nlme")
  for (ns in req.namespaces) {
    if (!requireNamespace(ns, quietly = TRUE)) {
      warning("The ", ns, " package is required for this function")
      return(NULL)
    }
  }
  
  ## rescaling the data
  xd <- as.Date(x, format = "%j")
  xm <- as.numeric(strftime(xd, "%m"))
  ref <- c(
    stats::median(y[which(xm %in% baseline.months)], na.rm = TRUE),
    y[which.max(y)]
  )
  rvalue <- stats::approx(
    x = ref,
    y = c(0, 1),
    xout = y
  )
  rvalue <- rvalue$y
  
  ## predicting cardinal dates
  predicted.dates <- .pa_predict_cardinal_dates(
    df = data.frame(
      doy = x,
      rvalue = rvalue
    ),
    model = model,
    prior.means = prior.means,
    prior.vars = prior.vars
  )
  
  ## correcting for bias
  predicted.dates <- predicted.dates - bias.correction
  return(predicted.dates)
}

#' @rdname pa_cardinal_dates
#' @export
pa_cardinal_dates.Date <- function(x, 
                                   y,
                                   baseline.months = c(1:3, 12),
                                   model = c('none', "card3", "scard3", "agauss", "harmonic"),
                                   prior.means,
                                   prior.vars,
                                   bias.correction,
                                   ...){
  x <- as.numeric(strftime(x, "%j"))
  pa_cardinal_dates(x = x, 
                    y = y, 
                    baseline.months = baseline.months,
                    model = model, 
                    prior.means = prior.means, 
                    prior.vars = prior.vars,
                    bias.correction = bias.correction)
  
}


#' @param ... ignored
#' @rdname pa_cardinal_dates
#' @export

pa_cardinal_dates.veg.index <- function(x,
                                        y = NULL,
                                        baseline.months = c(1:3, 12),
                                        model = c('none', "card3", "scard3", "agauss", "harmonic"),
                                        prior.means,
                                        prior.vars,
                                        bias.correction,
                                        ...){
  
  times <- stars::st_get_dimension_values(x, 'time')
  
  is.polygon <- try(sf::st_geometry_type(x) %in% c('POLYGON', 'MULTIPOLYGON'),
                    silent = TRUE)
  
  if (inherits(is.polygon, 'try-error')){
    is.polygon <- FALSE
    margins <- c('x', 'y')
  }else{
    is.polygon <- TRUE
    margins <- c('geometry')
  }
  
  vegetation.index <- attr(x, 'vegetation.index')
  x <- x[as.character(vegetation.index)]
  
  
  cpreds <- stars::st_apply(x,
                            MARGIN = margins,
                            FUN = function(y) {
                              y[is.nan(y)] <- NA
                              if (all(is.na(y)))
                                return(c(NA, NA, NA))
                              
                              preds <- pa_cardinal_dates(
                                x = times, 
                                y = y,
                                baseline.months = baseline.months,
                                model = model,
                                prior.means = prior.means,
                                prior.vars = prior.vars,
                                bias.correction = bias.correction
                              )
                              return(preds)
                            },
                            .fname = 'cardinal.dates')
  
  cpreds <- stars::st_set_dimensions(cpreds, which = 'cardinal.dates',
                                     values = c('tb', 'to', 'tm'))
  names(cpreds) <- 'cardinal.dates'
  
  if (!is.polygon){
    cpreds <- aperm(cpreds, c('x', 'y', 'cardinal.dates'))
  }else{
    cpreds <- aperm(cpreds, c('geometry', 'cardinal.dates'))
  }
  cpreds
}
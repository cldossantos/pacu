#' @title Downloads a met file using the apsimx package
#' @name pa_get_weather_sf
#' @rdname pa_weather_summary
#' @description This function retrieves weather data from
#' NASA Power and the Iowa Environmental Mesonet using the
#' apsimx package/
#' @param aoi a sf object
#' @param source the weather source from which the data
#'   should be retrieved. \sQuote{iem} = Iowa Environmental
#'   Mesonet, \sQuote{power} = NASA POWER. Defaults to
#'   \sQuote{iem}.
#' @param start.date first day to retrieve the weather data.
#'   Format should be \%Y-\%m-\%d.
#' @param end.date last day to retrieve the weather data.
#'   Format should be \%Y-\%m-\%d.
#' @return an object of class met
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "pacu")
#' area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'))
#' weather.met <- pa_get_weather_sf(aoi = area.of.interest,
#'                                start.date = '1990-01-01',
#'                                end.date = '2020-12-31')
#' }
#'
#'
pa_get_weather_sf <- function(aoi,
                              source = c('none', 'iem', 'power'),
                              start.date='1990-01-01',
                              end.date='2021-12-31') {

  source <- match.arg(source)
  if (source == 'none'){
    stop('Please choose the source of weather data.')
  }

  ## Since these packages are imported this is not necessary
  req.namespaces <- c('apsimx', 'sf')
  for (ns in req.namespaces) {
    if(!requireNamespace(ns, quietly = TRUE)){
      warning('The ', ns, ' package is required for this function')
      return(NULL)
    }
  }



  .check.date <- try(as.Date(c(start.date, end.date)), silent = TRUE)
  if(any(is.na(.check.date)) || inherits(.check.date, 'try-error'))
    stop('Dates are not in the correct format. Please enter dates as %Y-%m-%d')

  ## Exact coordinates are not needed for weather
  lonlat <- sf::st_bbox(sf::st_transform(aoi, crs = 4326))[1:2]


  met <- switch (source,
                 iem = apsimx::get_iem_apsim_met(lonlat = lonlat, dates = c(start.date, end.date)),
                 power = apsimx::get_power_apsim_met(lonlat = lonlat, dates = c(start.date, end.date))
  )


  ## Check met object
  apsimx::check_apsim_met(met)

  ## Replace negative values in radiation (if any) with NA
  ## I'm assuming this fixes potential problems
  if(any(met$radn < 0) || any(is.na(met$radn))){
    met$radn <- ifelse(met$radn < 0, NA, met$radn)
    met <- apsimx::impute_apsim_met(met)
  }

  return(met)

}


#'
#' @title Retrieve vegetation indices using Google Earth Engine
#' @name pa_get_gee_indices
#' @description Retrieve vegetation indices using Google Earth Engine
#' @param shapefile.path file path to the shapefile used to filter satellite images
#' @param out.path file path where the satellite data will be saved
#' @param project.name project name registered in Google Earth Engine
#' @param first.year beginning of time window to filter the satellite images
#' @param last.year end of time window to filter the satellite images
#' @details
#' The `pa_get_gee_indices()` function is a script that runs a python script to interact with
#' Google Earth Engine. This script will filter satellite images matching the
#' shapefile provided to the function and, also, the year range.
#' @noRd
#' @examples
#' \dontrun{
#' ## for pa_get_gee_indices examples see pacu vignette
#' }
#'
pa_get_gee_indices <- function(shapefile.path, out.path, project.name, first.year = 2000, last.year = 2023) {
  if(.Platform$OS.type == 'unix'){
    python <- 'python3'
  }else{
    python <- 'python'
  }
  python.dir <- system.file("python", package = "pacu")
  python.src <- file.path(python.dir, 'gee-indices.py')

  cmd.string <-  sprintf('%1$s %2$s %3$s %4$s %5$s %6$s %7$s',
                         python,
                         python.src,
                         project.name,
                         shapefile.path,
                         out.path,
                         first.year,
                         last.year)
  system(cmd.string, wait = TRUE)
}


#'
#' @title Plot vi curves and and summaries of area under the curve
#' @name pa_plot_vi_curve
#' @description Plot the vi curve and n
#' @param vi.data a vector containing the vegetation index data
#' @param dates a vector containing dates or string convertible to date
#' @param start day of the year to start computing the area under the curve. Defaults to 1.
#' @param end day of the year to finish computing the area under the curve. Defaults to 365.
#' @param tgt.year which year to focus and compare to the historical mean. Defaults to the last year in the data set.
#' @param date.format a strong representing date formatting, in case the \sQuote{dates} argument needs to be converted
#' @details
#' The `pa_plot_vi_curve()` function will summarize the vi data, calculate
#' the area under the curve and rank the target year among the other years. This
#' can be used to contextualize a given crop year.
#' @noRd
#' @examples
#' \dontrun{
#' ## for examples, see pacu vignette
#' }
#'
pa_plot_vi_curve <- function(vi.data,
                             dates,
                             tgt.year = 'last',
                             start = 1,
                             end = 365,
                             vi.label = 'Vegetation Index',
                             date.format = '%Y-%m-%d'){

  year <- vi <- predicted <- NULL

  req.namespaces <- c('ggplot2', 'patchwork', 'mgcv')
  for (ns in req.namespaces) {
    if(!requireNamespace(ns, quietly = TRUE)){
      warning('The ', ns, ' package is required for this function')
      return(NULL)
    }
  }

  if (!inherits(dates, c('POSIXct', 'POSIXt'))) {
    dates <-  try(as.Date(dates, format = date.format), silent = TRUE)
    if (inherits(dates, 'try-error') || any(is.na(dates))){
      stop('dates could not be converted to a valid date format')
    }
  }
  if ( length(vi.data) != length(dates)) {
    stop('length of dates and vi.data must match')
  }

  res <- list()
  aucs <- list()

  raw.data <- data.frame(dates = dates,
                         vi = vi.data)

  raw.data$dates <- as.POSIXct(raw.data$dates)
  raw.data$doy <- as.numeric(strftime(raw.data$dates, '%j'))
  raw.data$year <- as.numeric(strftime(raw.data$dates, '%Y'))

  if(tgt.year != 'last' && (!is.numeric(tgt.year) || !any(grepl(tgt.year, raw.data$year))))
    stop('tgt.year has to be last or a year in the data set')

  if(tgt.year == 'last'){ tgt.year <- max(unique(raw.data$year))}

  raw.data$year <- as.factor(raw.data$year)
  for(y in unique(raw.data$year)){
    yearly.raw.data <- subset(raw.data, year == y)
    fit.gam <- try(mgcv::gam(vi ~ s(doy), data = yearly.raw.data), silent = TRUE)
    if(inherits(fit.gam, 'try-error')) {next}
    preds <- data.frame(doy = 1:365)
    preds$year <- y
    preds$predicted <- mgcv::predict.gam(fit.gam, newdata = preds)
    auc <- stats::integrate(function(i) mgcv::predict.gam(fit.gam, newdata = list(doy = i)),
                     lower = start, upper = end)
    res <- append(res, list(preds))
    aucs <- append(aucs, list(data.frame(year = y, auc = auc$value)))
  }
  res <- do.call(rbind, res)
  aucs <- do.call(rbind, aucs)

  lw.year <-aucs$year[which.min(aucs$auc)]
  hg.year <-aucs$year[which.max(aucs$auc)]

  res <- subset(res, year %in% c(tgt.year, lw.year, hg.year))
  res$year <- factor(res$year, ordered = TRUE, levels = unique(c( lw.year, tgt.year,hg.year)))
  aucs$year <- factor(aucs$year, ordered = TRUE, levels = unique(c( lw.year, tgt.year,hg.year)))
  raw.data$year <- factor(raw.data$year, ordered = TRUE, levels = unique(c(lw.year, tgt.year, hg.year)))


  cmaps <- c('darkolivegreen3',  'tomato1', 'steelblue3','navyblue')
  names(cmaps) <- c('historical', 'record minimum', 'record maximum', tgt.year)

  res$date <- as.Date(res$doy, '%j', origin = as.Date('2019-12-31'))
  raw.data$date <- as.Date(raw.data$doy, '%j', origin = as.Date('2019-12-31'))
  start.date <- as.Date(start, '%j', origin = as.Date('2019-12-31'))
  end.date <- as.Date(end, '%j', origin = as.Date('2019-12-31'))


  ggplot2::ggplot()+
    ggplot2::geom_density(data = aucs, ggplot2::aes(x = auc), alpha = 0.5,  fill = 'darkolivegreen3')+
    ggplot2::geom_vline(data = aucs, ggplot2::aes(xintercept = auc[match(tgt.year, year)]), linetype = 2)+
    ggplot2::geom_text(data = aucs,ggplot2::aes(x = auc[match(tgt.year, year)], y = Inf, label = tgt.year), vjust = 1, hjust = 1)+
    ggplot2::labs(x = paste('Area under the', vi.label ,'curve'), y = '')+
    ggplot2::xlim(min(aucs$auc) * 0.8, max(aucs$auc) * 1.2)+
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()) +

    ggplot2::ggplot()+
    ggplot2::geom_point(data = subset(raw.data, year == tgt.year), ggplot2::aes( x = date, y = vi, fill = names(cmaps)[4]), pch = 21, cex = 2)+
    ggplot2::geom_point(data = subset(raw.data, year == lw.year), ggplot2::aes( x = date, y = vi, fill = names(cmaps)[2]), pch = 21, cex = 2)+
    ggplot2::geom_point(data = subset(raw.data, year == hg.year), ggplot2::aes( x = date, y = vi, fill = names(cmaps)[3]), pch = 21, cex = 2)+
    ggplot2::geom_ribbon(data = subset(res, date > start.date & date < end.date &  year == tgt.year),
                ggplot2::aes( x = date, ymin = -Inf, ymax = predicted, fill = names(cmaps)[4]), alpha = 0.4)+
    ggplot2::geom_ribbon(data = subset(res, date > start.date & date < end.date &  year == lw.year),
                ggplot2::aes( x = date, ymin = -Inf, ymax = predicted, fill = names(cmaps)[2]), alpha = 0.4)+
    ggplot2::geom_ribbon(data = subset(res, date > start.date & date < end.date &  year == hg.year),
                ggplot2::aes( x = date, ymin = -Inf, ymax = predicted, fill = names(cmaps)[3]), alpha = 0.4)+
    ggplot2::geom_line(data = subset(res, year == tgt.year), ggplot2::aes( x = date, y = predicted, col = names(cmaps)[4]))+
    ggplot2::geom_line(data = subset(res, year == lw.year), ggplot2::aes( x = date, y = predicted, col = names(cmaps)[2]))+
    ggplot2::geom_line(data = subset(res, year == hg.year), ggplot2::aes( x = date, y = predicted, col = names(cmaps)[3]))+
    ggplot2::geom_label(data = subset(aucs, year %in% res$year), ggplot2::aes(x = as.Date(-Inf), y = Inf,
                                                            label = paste('AUC:', round(auc, 1))),
               vjust = 1, hjust = 0)+
    ggplot2::scale_fill_manual(values = cmaps)+
    ggplot2::scale_color_manual(values = cmaps)+
    ggplot2::scale_x_date(date_labels = '%b-%d')+
    ggplot2::labs(x = 'Date', y = vi.label, col = '', fill = '')+
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~year, ncol = 1, strip.position = 'right') +

    patchwork::plot_layout(guides = 'collect', nrow = 1,
                widths = c(0.3, 0.7)) &
    ggplot2::theme(panel.spacing = ggplot2::unit(0, 'line'),
          strip.background = ggplot2::element_rect(fill = 'transparent', colour = 'transparent'),
          legend.position = 'top',
          legend.direction = 'horizontal') &
 patchwork::plot_annotation(caption = sprintf('AUC: Area under the curve\nCalculated from %1$s to %2$s',
                                      strftime(start.date, '%b-%d'),
                                      strftime(end.date, '%b-%d')))

}


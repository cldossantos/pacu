#'
#' @title Create an interpolated treatment object from as applied data
#' @description Create an interpolated treatment object from as applied data
#' @name pa_trial
#' @noRd
#' @param input an sf object containing the as applied treatment
#' @param data.columns tbd
#' @param data.units tbd
#' @param grid an sf object containing the prediction grid.
#'   If the user is processing yield data coming from a
#'   research trial (i.e. follows a trial design), the user
#'   can pass the sf object containing the trial design
#'   information to this argument. If the argument
#'   \sQuote{formula} contains any predictions, the
#'   predictor should be included in the sf object supplied
#'   to this argument. polygons for which the predictions
#'   generated
#' @param algorithm algorithm used to generate the yield
#'   object.
#' @param formula formula defining the relationship between
#'   the dependent and independent variables. If the
#'   dependent variable is a linear function of the
#'   coordinates, the formula can be \sQuote{z ~ X + Y}. If
#'   the dependent variable is modeled only as a function of
#'   the mean spatial process, the formula can be \sQuote{z
#'   ~ 1}. If no formula is supplied, it defaults to
#'   \sQuote{z ~ 1}.
#' @param overlap.threshold a fraction threshold to remove
#'   observations when there is overlap between the
#'   vehicular polygons. A value of 0 does not remove any
#'   observations. A value of 1 removes all observations
#'   that overlap even minimally with neighboring
#'   observations.
#' @param var.label optional string to name the final
#'   product. Defaults to \sQuote{yield}.
#' @param boundary optional sf object representing the
#'   field's outer boundary. If it not supplied, the
#'   function attempts to generate a boundary from the
#'   observed points.
#' @param clean whether to clean the raw data based on
#'   distance from the field edge and global standard
#'   deviation.
#' @param clean.sd standard deviation above which the
#'   cleaning step will remove data. Defaults to 3.
#' @param clean.edge.distance distance, in meters, from the
#'   field edge above which the cleaning step will remove
#'   data. Defaults to 0.
#' @param lbs.per.bushel a numeric value representing the
#'   number of pounds in a bushel. For example, 60 for
#'   soybean and 56 for corn. This argument can be ommitted
#'   when the input and output units are in the metric
#'   system. It is necessary otherwise.
#' @param moisture.adj an optional numeric value to set the
#'   moisture value to which the yield map predictions
#'   should be adjusted. For example,  15.5 for corn, and
#'   13.0 for soybean. If NULL, the function will adjust the
#'   moisture to the average moisture of the field.
#' @param unit.system a string representing the unit system
#'   to be used in the function output. If
#'   \sQuote{standard}, the function output will be in
#'   bushel/acre. Alternatively, if \sQuote{metric}, outputs
#'   will be in metric tonnes/hectare.
#' @param cores the number of cores used in the operation
#' @param verbose whether to print function progress.
#'   \sQuote{FALSE or 0} will suppress details. \sQuote{TRUE
#'   or 1} will print a progress bar. \sQuote{>1} will print
#'   step by step messages.
#' @param ... additional arguments to be passed
#'   \link[gstat]{krige} and \link[gstat]{idw}
#' @details This function will follow the steps in the
#'   selected algorithm to produce a yield map from the raw
#'   data.
#' @return an object of class yield
#' @author Caio dos Santos and Fernando Miguez
#' @examples
#' \dontrun{
#' ## tbd
#' }
#'
pa_trial <- function(input,
                     data.columns = NULL,
                     data.units = NULL,
                     grid = NULL,
                     algorithm = c('none', 'simple', 'ritas'),
                     formula = NULL,
                     overlap.threshold = 0.5,
                     var.label = NULL,
                     boundary = NULL,
                     clean = FALSE,
                     clean.sd = 3,
                     clean.edge.distance = 0,
                     moisture.adj = NULL,
                     lag.adj = 0,
                     na.to.zero = TRUE,
                     unit.system = c('none', 'metric', 'standard'),
                     remove.crossed.polygons = FALSE,
                     cores = 1L,
                     verbose = TRUE,
                     ...) {


  algorithm <- match.arg(algorithm)
  unit.system <- match.arg(unit.system)
  pb <- ifelse(verbose == 1, TRUE, FALSE)
  verbose <- ifelse(verbose > 1, 1, 0)


  if (algorithm == 'none')
    stop('Please choose between the simple and ritas algorithms')

  if (unit.system == 'none')
    stop('Please choose between the metric and standard unit systems')

  if (any(!(data.columns[!is.na(data.columns)] %in% names(input))))
    stop('One or more of the data.columns supplied does not match any columns in the input.')


  if(verbose) cat("Starting... \n")

  crt.crs <- sf::st_crs(input)
  if(is.na(crt.crs)) {
    if (verbose) cat("No CRS found. Defaulting to EPSG:4326 \n")
    sf::st_crs(input) <- 'epsg:4326'
  }

  input <- pa_2utm(input, verbose) ## This step appears to be quick

  if(!is.null(boundary)){
    if (sf::st_crs(boundary) != sf::st_crs(input)) {
      boundary <- sf::st_transform(boundary, sf::st_crs(input))
    }
  }



  if(!is.null(grid)){

    if (!inherits(grid, 'sf')) {
      grid <- sf::st_as_sf(grid)
    }

    if(is.na(sf::st_crs(grid))) {
      if (verbose) cat("No CRS found for grid. Defaulting to EPSG:4326 \n")
      sf::st_crs(grid) <- 'epsg:4326'
    }

    if (sf::st_crs(grid) != sf::st_crs(input)) {
      grid <- sf::st_transform(grid, sf::st_crs(input))
    }

    if (!all(c('X', 'Y') %in% names(grid))) {
      grid <- cbind(grid, suppressWarnings(sf::st_coordinates(sf::st_centroid(grid))))
    }

  }


  if (is.null(formula)) {
    form <- formula(z ~ 1)
  }else{
    form <- formula(formula)
  }



    exp.vars <-  all.vars(form)
    exp.vars <- exp.vars[exp.vars != 'z']

    if (length(exp.vars) > 0) {
      if (is.null(grid))
        stop('When formula contains explanatory variables, grid must be supplied.')

      if (!all(exp.vars %in% names(grid)))
        stop('One or more of the explanatory variables are not present in the grid.')
    }


  if (algorithm == 'simple') {

    if(pb) {
      progress.bar <- utils::txtProgressBar(min = 0, max = 5, style = 3, initial = -1)
      on.exit(close(progress.bar))
    }

    if (is.null(var.label)) var.label <- data.columns[1]

    tgt <- input[[data.columns]]


    if(pb)
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)

    if(pb)
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)

    tgt <- data.frame(tgt = tgt)
    tgt <- cbind(tgt, sf::st_geometry(input))
    tgt <- st_as_sf(tgt)

    if (!is.null(grid)) {
      if (!is.null(boundary)) {
        grid <- sf::st_intersection(grid, boundary)
      }
      f.grid <- stats::aggregate(tgt, grid, FUN = function(x) mean(x, na.rm = TRUE))
      f.grid <- stats::na.omit(f.grid)
      tgt <- f.grid
    }

    app.pols <- tgt
    names(app.pols) <- c('mass', 'geometry')
    st_geometry(app.pols) <- 'geometry'

    if(pb)
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)
  }

  if (algorithm == 'ritas') {

    ## handling units and column names
    exp.order <- c('treatment','interval', 'angle', 'width', 'distance')
    if (is.null(data.columns)) data.columns <- rep(NA, 5)
    if (is.null(data.units)) data.units <- rep(NA, 5)
    if(is.null(names(data.columns))) names(data.columns) <- exp.order
    if(is.null(names(data.units))) names(data.units) <- exp.order
    data.units <- data.units[exp.order]
    data.columns <- data.columns[exp.order]
    if (is.null(var.label)) var.label <- data.columns['treatment']

    interval <- .pa_get_variable(input, 'interval', data.units['interval'], data.columns['interval'], verbose)
    if (is.null(interval)) {
      time.col <- .pa_get_variable_columns(input, 'time', verbose)
      if (!is.null(time.col)){
        time <- input[[time.col]]
        interval <- .pa_time2interval(time)
        interval <- .pa_enforce_units(interval, 'time')
        input$interval <- interval
      }
    }

    ## keeping track of the units. this is intend this to prevent mistakes.
    treatment <- input[[data.columns['treatment']]]

    angle <- .pa_get_variable(input, 'angle', data.units['angle'], data.columns['angle'], verbose)
    if(is.null(angle)) {
      if(verbose) cat('Trajectory angle not found. estimating it from geographical coordinates.\n')
      angle <- .pa_estimate_angle(sf::st_geometry(input))
    }
    swath <- .pa_get_variable(input, 'width', data.units['width'], data.columns['width'], verbose)
    distance <- .pa_get_variable(input, 'distance', data.units['distance'], data.columns['distance'], verbose)

    ## checking that all necessary variables were found
    not.found <- sapply(list(treatment, interval, angle, swath, distance), is.null)
    if(any(not.found)) {
      not.found.i <- which(not.found == TRUE)
      stop('unable to find column(s): ', paste(exp.order[not.found.i], collapse = ', '))
    }

    ### might need to change this after testing
    if(pb) {
      progress.bar <- utils::txtProgressBar(min = 0, max = 7, style = 3)
      on.exit(close(progress.bar))
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)
    }

    ## now, we can drop the units because we know which units are
    ## in and out of each operation
    swath <- units::drop_units(swath)
    distance <- units::drop_units(distance)
    angle <- units::drop_units(angle)

    if(pb)
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)

    trt.pols <- pa_make_vehicle_polygons(sf::st_geometry(input),
                                       swath,
                                       distance,
                                       angle,
                                       cores = cores,
                                       verbose = verbose)
    trt.pols <- sf::st_as_sf(trt.pols)
    trt.pols$treatment <- treatment

    if(pb)
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)


    if(pb)
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)

    app.pols <- pa_apportion_mass(polygons =  sf::st_geometry(trt.pols),
                               mass.vector = trt.pols$treatment,
                               remove.empty.cells = FALSE,
                               cores = cores,
                               sum = TRUE,
                               verbose = verbose)

    boundary <- .pa_field_boundary(sf::st_geometry(input))
    if (na.to.zero){
    out.boundary <- sf::st_covered_by(app.pols, boundary)
    out.boundary <- as.numeric(out.boundary)
    app.pols <- app.pols[!(is.na(out.boundary) & is.na(app.pols$mass)), ]
    app.pols$mass[is.na(app.pols$mass)] <- 0
    }else{
      app.pols <- stats::na.omit(app.pols)
    }

  }

  ## the following steps are the same regardless of the algorithm

  if (!is.null(grid)){
    app.pols <- suppressWarnings(sf::st_join(app.pols, grid, join = sf::st_intersects, left = TRUE, largest = TRUE))
  }



  if(pb)
    utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)



    preds <- app.pols['mass']
    preds <- stats::na.omit(preds)
    names(preds) <- c(var.label, 'geometry')
    sf::st_geometry(preds) <- 'geometry'
    preds <- preds[c(var.label, 'geometry')]

    # preds <- stats::aggregate(preds,
    #                           sf::st_geometry(grid),
    #                           mean,
    #                           na.rm = TRUE)
    preds <- .pa_areal_weighted_average(preds,
                                        sf::st_geometry(grid),
                                        var = var.label,
                                        fn = sf::st_intersects,
                                        sum = FALSE,
                                        cores = cores)



  if(pb)
    utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)

  attr(preds, 'units') <- data.units['treatment']
  attr(preds, 'algorithm') <- algorithm
  attr(preds, 'resp') <- var.label

  res <- preds

  if(pb)
    utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)

  if (verbose)
    cat('Processing complete!\n')

  class(res) <- c('treatment', class(res))
  return(res)

}

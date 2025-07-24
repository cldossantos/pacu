#'
#' @title EXPERIMENTAL FUNCTION - Create an interpolated trial object from as-applied data
#' @description EXPERIMENTAL FUNCTION - Create an interpolated trial object from as-applied data
#' @name pa_trial
#' @rdname pa_trial
#' @param input an sf object containing the as applied trial
#' @param data.columns When algorithm is \sQuote{simple},
#'   this argument should be a vector of length one indicating which 
#'   column contains the \sQuote{trial or as-applied} data. 
#'   When algorithm is \sQuote{ritas}, an optional
#'   named vector with the column names for the variables
#'   \sQuote{trial, angle, swath,
#'   distance}. If a an unnamed vector is supplied, the
#'   vector is assumed to be in this order. The default is
#'   NULL, in which case the function attempts to guess the
#'   columns by using a dictionary of possible guesses.
#'   The column indicating the \sQuote{trial} information is not guessed, 
#'   as there are too many possible options 
#'   (seeds, fertilizer, soil amendments, etc).
#' @param data.units When algorithm is \sQuote{simple},
#'   should be a vector of length one indicating the units
#'   of the trial column and the moisture column. Common
#'   values would be \sQuote{c('kg N/ha', 'seeds/acre')}. When
#'   algorithm is \sQuote{ritas}, an optional named vector
#'   with strings representing units for the variables
#'   \sQuote{trial, angle, swath,
#'   distance}. If a an unnamed vector is supplied, the
#'   vector is assumed to be in this order. A typical value
#'   for this argument would be \sQuote{c(trial = 'kg N/ha',
#'    angle = 'degreeN',
#'   width = 'ft', distance = 'ft')}. Please see
#'   \link[units]{valid_udunits} for help with specifying
#'   units. The default is NULL, in which case the function
#'   attempts to guess the units according to the values of
#'   the variable. The units of \sQuote{trial} are not guessed, 
#'   as there are too many possible options (seeds, fertilizer, soil amendments, etc).
#' @param grid an sf object containing the prediction grid.
#'   If the user is processing as-applied data coming from a
#'   research trial (i.e. follows a trial design), the user
#'   can pass the sf object containing the trial design
#'   information to this argument. 
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
#' @param var.label optional string to name the final
#'   product. Defaults to \sQuote{as.applied}.
#' @param boundary optional sf object representing the
#'   field's outer boundary. If it not supplied, the
#'   function attempts to generate a boundary from the
#'   observed points. 
#' @param smooth.method the smoothing method to be used. If
#'   \sQuote{none}, no smoothing will be conducted. If
#'   \sQuote{idw}, inverse distance weighted interpolation
#'   will be conducted. If \sQuote{krige}, kriging will be
#'   conducted.
#' @param conversion.factor a conversion factor by which the 
#' input trial data will be multiplied. This is useful for 
#' cases in which the user wants the output in different units from 
#' the input. A trivial example is a fertilizer trial in which 
#' the fertilizer contained in the input is only 50% of the total mass. 
#' In this case, conversion.factor should be set to 0.5.
#' @param out.units units of the output after being multiplied by
#' the conversion factor. If conversion.factor is 1 and out.units
#' is NULL, out.units will default to the units of the trial input.
#' @param na.to.zero whether areas in which the trial applicator has not covered
#' should be assigned a value of zero. This is only effective when \sQuote{algorithm}
#' is \sQuote{ritas}. Defaults to TRUE when \sQuote{algorithm} is \sQuote{ritas}. 
#' @param cores the number of cores used in the operation
#' @param steps whether to return the intermediate steps 
#' of the trial processing algorithm
#' @param verbose whether to print function progress.
#'   \sQuote{FALSE or 0} will suppress details. \sQuote{TRUE
#'   or 1} will print a progress bar. \sQuote{>1} will print
#'   step by step messages.
#' @param ... additional arguments to be passed
#'   \link[gstat]{krige} and \link[gstat]{idw}
#' @details This function will follow the steps in the
#'   selected algorithm to produce a  map of as-applied trial from the raw
#'   data.
#' @return an object of class trial
#' @author Caio dos Santos and Fernando Miguez
#' @export
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
                     var.label = 'as.applied',
                     boundary = NULL,
                     smooth.method = c('none', 'krige', 'idw'),
                     formula = NULL,
                     out.units = NULL,
                     conversion.factor = 1,
                     na.to.zero = ifelse(algorithm == 'ritas', TRUE, FALSE),
                     cores = 1L,
                     steps = FALSE,
                     verbose = TRUE,
                     ...) {
  
  
  algorithm <- match.arg(algorithm)
  pb <- ifelse(verbose == 1, TRUE, FALSE)
  smooth.method <- match.arg(smooth.method)
  verbose <- ifelse(verbose > 1, 1, 0)
  mass <- NA
  
  s.wrns <-  get("suppress.warnings", envir = pacu.options)
  s.msgs <-  get("suppress.messages", envir = pacu.options)
  
  if (algorithm == 'none')
    stop('Please choose between the simple and ritas algorithms')
  
  if (any(!(data.columns[!is.na(data.columns)] %in% names(input))))
    stop('One or more of the data.columns supplied does not match any columns in the input.')
  
  if (is.null(grid))
    stop('Grid is needed to process trial application data. Usually this is simply the experimental design.')
  
  if (is.null(out.units) && conversion.factor != 1)
    stop('When conversion.factor is different from 1, data.units and out.units are needed')
  
  
  
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
  
  
  if (is.null(formula)) {
    form <- formula(z ~ 1)
  }else{
    form <- formula(formula)
  }
  
  if(!is.null(formula) && smooth.method != 'krige') {
    stop('formula should only be used when smooth.method = krige')
  }
  
  
  
  trial.vars <- NULL
  if(!is.null(grid)){
    
    if (inherits(grid, 'trial')){
      trial.vars <- attr(grid$trial, 'resp')
      grid <- grid[['trial']]
    }
    
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
  
  exp.vars <-  all.vars(form)
  exp.vars <- exp.vars[exp.vars != 'z']
  exp.vars <- unique(c(exp.vars, trial.vars))
  
  if (length(exp.vars) > 0) {
    if (is.null(grid))
      stop('When formula contains explanatory variables, grid must be supplied.')
    
    if (!all(exp.vars %in% names(grid)))
      stop('One or more of the explanatory variables are not present in the grid.')
  }
  
  
  
  if (algorithm == 'simple') {
    
    if (is.null(data.columns))
      stop('When algorithm is simple, data.columns requires a string pointing to the ',
           'trial column.')
    
    if (length(data.columns) > 1){
      stop('When algorithm is simple, data.columns only takes one value.')
    }
    
    if (na.to.zero){
      if(!s.wrns)
        warning('When algorithim is "simple", na.to.zero has no effect.')
    }
    
    if(pb) {
      progress.bar <- utils::txtProgressBar(min = 0, max = 5, style = 3, initial = -1)
      on.exit(close(progress.bar))
    }
    
    exp.order <- c('trial')
    
    if (any(!(names(data.columns) %in% exp.order)))
      stop('One or more of the columns provided to data.columns',
           'do not match the expected inputs.')
    
    if (is.null(data.columns)) data.columns <- rep(NA, 1)
    if (is.null(data.units)) data.units <- rep(NA, 1)
    if(length(data.columns) < 1) length(data.columns) <- 1
    if(length(data.units) < 1) length(data.units) <- 1
    if(is.null(names(data.columns))) names(data.columns) <- exp.order
    if(is.null(names(data.units))) names(data.units) <- exp.order
    data.units <- data.units[exp.order]
    data.columns <- data.columns[exp.order]
    
    if (is.null(out.units) && conversion.factor == 1)
      out.units <- data.units['trial']
    
    
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
    
  sbs <- list(sf::st_geometry(input), sf::st_geometry(grid))
  steps.names <- c('initial.geometries', 'grid')
  names(sbs) <- steps.names
    if(pb)
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)
  }
  
  
  if (algorithm == 'ritas') {
    
    sbs <- list()
    
    ## handling units and column names
    exp.order <- c('trial', 'angle', 'width', 'distance')
    if (is.null(data.columns)) data.columns <- rep(NA, 4)
    if (is.null(data.units)) data.units <- rep(NA, 4)
    if(is.null(names(data.columns))) names(data.columns) <- exp.order
    if(is.null(names(data.units))) names(data.units) <- exp.order
    data.units <- data.units[exp.order]
    data.columns <- data.columns[exp.order]
    if (is.null(var.label)) var.label <- data.columns['trial']
    
    if (is.null(out.units) && conversion.factor == 1)
      out.units <- data.units['trial']
    
    
    ## keeping track of the units. this is intend this to prevent mistakes.
    trial <- input[[data.columns['trial']]]
    
    angle <- .pa_get_variable(input, 'angle', data.units['angle'], data.columns['angle'], verbose)
    if(is.null(angle)) {
      if(verbose) cat('Trajectory angle not found. estimating it from geographical coordinates.\n')
      angle <- .pa_estimate_angle(sf::st_geometry(input))
    }
    swath <- .pa_get_variable(input, 'width', data.units['width'], data.columns['width'], verbose)
    distance <- .pa_get_variable(input, 'distance', data.units['distance'], data.columns['distance'], verbose)
    
    ## checking that all necessary variables were found
    not.found <- sapply(list(trial, angle, swath, distance), is.null)
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
    
    sbs[[length(sbs) + 1]] <- sf::st_geometry(input)
    
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
    
    sbs[[length(sbs) + 1]] <- sf::st_geometry(trt.pols)
    trt.pols <- sf::st_as_sf(trt.pols)
    trt.pols$trial <- trial
    
    if(pb)
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)
    
    
    if(pb)
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)
    
    app.pols <- pa_apportion_mass(polygons =  sf::st_geometry(trt.pols),
                                  mass.vector = trt.pols$trial,
                                  remove.empty.cells = FALSE,
                                  cores = cores,
                                  sum = TRUE,
                                  verbose = verbose)
    
    
    if (is.null(boundary)){
      boundary <- .pa_field_boundary(sf::st_geometry(input))
    }
    
    if (na.to.zero){
      out.boundary <- sf::st_covered_by(app.pols, boundary)
      out.boundary <- as.numeric(out.boundary)
      app.pols <- app.pols[!(is.na(out.boundary) & is.na(app.pols$mass)), ]
      app.pols$mass[is.na(app.pols$mass)] <- 0
    }else{
      app.pols <- stats::na.omit(app.pols)
    }
    
    sbs[[length(sbs) + 1]] <- sf::st_geometry(app.pols)
    sbs[[length(sbs) + 1]] <- sf::st_geometry(grid)
    
    
  steps.names <- c('initial.points', 'vehicle.polygons', 'apportioned.polygons', 
                   'grid')
  names(sbs) <- steps.names
  }
  
  ## the following steps are the same regardless of the algorithm
  
  if (!is.null(grid)){
    app.pols <- suppressWarnings(sf::st_join(app.pols, grid, join = sf::st_intersects, left = TRUE, largest = TRUE))
  }
  
  
  
  if(pb)
    utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)
  
  
  if (smooth.method == 'krige'){
    app.pols$mass[app.pols$mass == 0] <- 1e-6
    app.pols$z <- app.pols$mass
    app.pols <- cbind(app.pols, suppressWarnings(sf::st_coordinates(sf::st_centroid(app.pols))))
    app.pols <- stats::na.omit(app.pols)
    preds <- .pa_predict(formula = form,
                         smooth.method = smooth.method,
                         df = app.pols,
                         new.df = grid,
                         cores = cores,
                         fun = 'none',
                         verbose = verbose,
                         ...)
    
    variogram.model <- preds[[2]]
    variogram <- preds[[3]]
    preds <- preds[[1]]
    predicted.var <- data.frame(preds$var1.pred, preds$var1.var)
    names(predicted.var) <- c(var.label, paste0(var.label,'.var'))
    predicted.var[[1]] <- predicted.var[[1]] * conversion.factor
    predicted.var[[2]] <- predicted.var[[2]] * (conversion.factor**2)
    preds <- cbind(preds, predicted.var)
    preds <- preds[c(var.label,  paste0(var.label,'.var'), 'geometry')]
    sf::st_geometry(preds) <- 'geometry'
    
    
    
  }
  
  if (smooth.method == 'idw'){
    
    if (form != formula(z ~ 1)){
      stop('The IDW smoothing method does not allow for predictors in the formula. The "formula" argument should be: z ~ 1')
    }
    
    app.pols$z <- app.pols$mass
    app.pols <- subset(app.pols, !is.na(mass))
    preds <- .pa_predict(formula = form,
                         smooth.method = smooth.method,
                         df = app.pols,
                         new.df = grid,
                         cores = cores,
                         verbose = verbose,
                         ...)
    
    variogram.model <- NULL
    variogram <- NULL
    preds <- preds[[1]]
    
    
    predicted.var <- data.frame(preds$var1.pred)
    names(predicted.var) <- var.label
    predicted.var[[1]] <- predicted.var[[1]] * conversion.factor
    preds <- cbind(preds, predicted.var)
    preds <- preds[c(var.label, 'geometry')]
    sf::st_geometry(preds) <- 'geometry'
    
  }
  
  if (smooth.method == 'none'){
    preds <- app.pols['mass']
    preds <- stats::na.omit(preds)
    if (!identical(sf::st_geometry(preds),
                   sf::st_geometry(grid))){
      preds <- .pa_areal_weighted_average(x = preds, 
                                          y = grid, 
                                          var = 'mass',
                                          fn = sf::st_intersects,
                                          sum = FALSE,
                                          cores = cores)
      preds <- rev(preds)
    }
    
    if (algorithm == 'ritas' && na.to.zero){
      preds$mass[is.na(preds$mass)] <- 0
    }
    
    preds <- stats::na.omit(preds)
    preds[[1]] <- preds[[1]] * conversion.factor
    names(preds) <- c(var.label, 'geometry')
    sf::st_geometry(preds) <- 'geometry'
    preds <- preds[c(var.label, 'geometry')]
    variogram.model <- NULL
    variogram <- NULL
    
  }
  
  if(pb)
    utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)
  
  col.order <- names(preds)
  preds$fid <- paste0('fid-', 1:nrow(preds))
  preds$fid <- as.factor(preds$fid)
  preds <- preds[c('fid', col.order)]
  
  out.units[is.na(out.units)] <- 'unknown'
  attr(preds, 'units') <- out.units
  attr(preds, 'algorithm') <- algorithm
  attr(preds, 'resp') <-  var.label
  attr(preds, 'smooth.method') <- smooth.method
  attr(preds, 'formula') <- form
  
  res <- list(trial = preds,
              variogram = variogram,
              variogram.model = variogram.model,
              steps = NULL)
  
  if (steps){
  res[['steps']] <- sbs
  }
  
  if(pb)
    utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1)
  
  if (verbose)
    cat('Processing complete!\n')
  
  class(res) <- c('trial', class(res))
  return(res)
  
}




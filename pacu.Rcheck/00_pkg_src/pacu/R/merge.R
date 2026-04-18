#' @title Merge trial objects
#' @description  Generic merge functionalities for pacu objects
#' @name merge
#' @rdname pa_merge
#' @param ... pa_trial objects
#' @exportS3Method base::merge
#' @return object of class "trial"
#' @export

merge.trial <- function(...){
  
  trials <-  list(...)
  units <- c() 
  algorithm <- c()
  resp <- c()
  smooth.method <- c()
  formula <- c()
  
  if (length(trials) < 2)
    stop('You need to provide at least two trial objects to be merged')
  
  resps <- sapply(trials, function(x) attr(x[['trial']], 'resp'))
  dr <- duplicated(resps)
  
  if(any(dr))
    stop('Two or more of your trial objects have the same variable name:', 
         paste(resps[which(dr)], collapse = ', '), '. Please make sure that variable names are unique.')
  
  
  trial.obj <- NULL
  variograms <- vector('list', length(trials))
  variogram.models <- vector('list', length(trials))
  
  for (i in 1:length(trials)){
    if (is.null(trial.obj))
      trial.obj <- sf::st_as_sf(trials[[1]][['trial']]['fid'])
    
    resp.i <- attr(trials[[i]][['trial']], 'resp')
    trial.obj <- sf::st_join(trial.obj,
                             trials[[i]][['trial']][resp.i],
                             join = sf::st_equals,
                             left = TRUE)
    
    units <- c(units, attr(trials[[i]][['trial']], 'units'))
    algorithm <- c(algorithm, attr(trials[[i]][['trial']], 'algorithm'))
    resp <- c(resp, attr(trials[[i]][['trial']], 'resp'))
    smooth.method <- c(smooth.method, attr(trials[[i]][['trial']], 'smooth.method'))
    formula <- c(formula, attr(trials[[i]][['trial']], 'formula'))
    
    
    variograms[[i]] <- trials[[i]][['variogram']]
    variogram.models[[i]] <- trials[[i]][['variogram.model']]
  }
  
  attr(trial.obj, 'units') <- units
  attr(trial.obj, 'algorithm') <- algorithm
  attr(trial.obj, 'resp') <- resp
  attr(trial.obj, 'smooth.method') <- smooth.method
  attr(trial.obj, 'formula') <- formula
  
  res <- list(trial = trial.obj,
              variogram = variograms,
              variogram.model = variogram.models,
              steps = NULL)
  
  class(res) <- c('trial', class(res))
  return(res)
}
#'
#' @title Request  vegetation index statistics from the Data
#'   Space Statistics API
#' @description  Request  vegetation index statistics from
#'   the Data Space Statistics API
#' @name pa_get_vi_stats
#' @rdname pa_get_vi_stats
#' @param aoi sf object used to filter satellite products
#' @param start.date beginning of the time window to filter
#'   satellite products. Date format should be \%Y-\%m-\%d.
#' @param end.date end of the time window to filter
#'   satellite products. Date format should be \%Y-\%m-\%d.
#' @param collection for now, it only supports 'sentinel2'.
#' @param vegetation.index vegetation index to be requested
#'   from the Data Space
#' @param agg.time aggregation time of the satellite
#'   products
#' @param by.feature logical, indicating whether the
#'   statistics should be retrieved by each polygon when
#'   multiple polygons are supplied in \sQuote{aoi}
#' @param url Copernicus Statistics API url. Do not change
#'   this unless Copernicus issues a new API
#' @details pa_get_vi_sentinel2 will use HTTP requests to
#'   communicate with the Data Space Statistics API and
#'   request areal statistics for the specified vegetation
#'   index
#' @return returns an object of class veg.index and stars
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "pacu")
#' area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'), quiet = TRUE)
#' ndvi <- pa_get_vi_stats(aoi = area.of.interest,
#'                         start.date = '2021-01-01',
#'                         end.date = '2021-12-31',
#'                         vegetation.index = 'ndvi')
#' }
#'

pa_get_vi_stats<- function(aoi,
                            start.date,
                            end.date,
                            collection = c('sentinel-2-l2a'),
                            vegetation.index = c('bsi', 'evi', 'gcvi', 'ndre', 'ndvi', 'reci'),
                            agg.time = c('P1D', 'P5D', 'P10D'),
                            by.feature = FALSE,
                            url = "https://sh.dataspace.copernicus.eu/api/v1/statistics"){


  agg.time <- match.arg(agg.time)
  vegetation.index = match.arg(vegetation.index)
  collection <- match.arg(collection)

  if(!inherits(aoi, 'sf'))
    stop('aoi must be an sf object')

  if(sf::st_crs(aoi)$input != 'EPSG:4326'){
    aoi <- sf::st_transform(aoi, 4326)
  }



  .check.date <- try(as.Date(c(start.date, end.date)), silent = TRUE)
  if(any(is.na(.check.date)) || inherits(.check.date, 'try-error'))
    stop('Dates are not in the correct format. Please enter dates as %Y-%m-%d')

  req.namespaces <- c('jsonlite', 'httr', 'sf', 'sftime')
  for (ns in req.namespaces) {
    if(!requireNamespace(ns, quietly = TRUE)){
      warning('The ', ns, ' package is required for this function')
      return(NULL)
    }
  }

  if("" %in% c(Sys.getenv('DATASPACE_CLIENTID'), Sys.getenv('DATASPACE_CLIENTSECRET')))
    stop('Dataspace Oauth client id or secret not registered in R envinronemt. Use initialize_oauth to register credentials to R environment.')


  ## cleaning attributes from aoi
  aoi <- sf::st_geometry(aoi)
  aoi <- sf::st_as_sf(aoi)

  if(by.feature == TRUE) {
    aoi$id <- 1:nrow(aoi)
  } else {
    aoi$id <- 1
  }

  ## Oauth2.0 credentials
  client.id <- Sys.getenv('DATASPACE_CLIENTID')
  client.secret <- Sys.getenv('DATASPACE_CLIENTSECRET')
  token <- .pa_get_dataspace_token(client.id, client.secret, TRUE)

  aoi.list <- split(aoi, aoi$id)

  res <- lapply(aoi.list, function(iaoi) {

    ## Building the json request
    body_json <- .pa_build_request_body(iaoi, start.date , end.date, vegetation.index, agg.time, collection)

    resp <- try(httr::POST(url = url,
                           body = body_json,
                           httr::add_headers(.headers = c(
                             'Content-Type' = "application/json",
                             'Accept' = "application/json",
                             'Authorization' = paste("Bearer", token$access_token))),
                           encode = 'json'),
                silent = TRUE)

    if(!inherits(resp, 'try-error')){

      if(httr::status_code(resp) != 200)
        resp <- httr::RETRY('POST',
                            url = url,
                            body = body_json,
                            httr::add_headers(.headers = c(
                              'Content-Type' = "application/json",
                              'Accept' = "application/json",
                              'Authorization' = paste("Bearer", token$access_token))),
                            encode = 'json')

      if(httr::status_code(resp) != 200){
        stop("There was an error with the request.\n")
      }
    }
    ## Sending request
    ## Parse the json response
    rj <- jsonlite::fromJSON(httr::content(resp, 'text'))
    ## Convert to data frame
    results <- as.data.frame(rj$data)
    ## Formatting the data frame
    results <-   jsonlite::flatten(results)
    results <- merge(iaoi, results)
    return (results)

  })

  res <- do.call(rbind, res)

  names(res) <- gsub('outputs.data.bands.B0.stats', vegetation.index, names(res))
  names(res) <- gsub('percentiles.50.0', 'median', names(res))

  res$time <- as.Date(res$interval.from)
  res <- res[c('time', 'id', grep(vegetation.index, names(res), value = TRUE))]
  res <- subset(res, !is.na(res[[3]]))
  res <- res[order(res$time, res$id), ]
  row.names(res) <- NULL
  geometry.col <- grep('geometry|^x$', names(res), value = TRUE)[1]

  res <- stars::st_as_stars(res, dims = c(geometry.col, 'time'))
  attr(res, 'vegetation.index') <- paste0(vegetation.index, '.mean')
  class(res) <- c('veg.index', 'stars')

  return(res)
}


#'
#' @title Register the Oauth2.0 credentials to the R environment
#' @description Register the Oauth2.0 credentials to the R environment
#' @name pa_initialize_oauth
#' @rdname pa_initialize_oauth
#' @param client_id client id used to authenticate the HTTP request
#' @param client_secret client secret used to authenticate the HTTP request
#' @details initialize_oauth registers the client id and secret to the machine's R environment
#' All the other functions that rely on authentication will search for the clients id ans secret in
#' the R environment. Do not share your R environment with others, as they will be able to read your client id and secret.
#' You can register at https://dataspace.copernicus.eu/news. Please see this section for how to create your Oauth2.0 client:
#' https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Overview/Authentication.html
#' @return NULL
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \dontrun{
#' pa_initialize_oauth('my-client-id', 'my-client-secret')
#' }
#'
pa_initialize_oauth <- function(client_id, client_secret) {

  ## Path to R env file
  renv_file <- file.path(Sys.getenv("HOME"), ".Renviron")

  ## If the file does not exists, we make one
  if(!file.exists(renv_file)) {
    file.create(renv_file)
  }

  ## Reading entries in the R environment
  renv_entries <- readLines(renv_file)

  # First, we can look for the client_id

  ##  If there is no client_id set, set one
  if(!any(grep('DATASPACE_CLIENTID', renv_entries))){

    renv_entries <- c(renv_entries,
                      paste0('DATASPACE_CLIENTID=', client_id))
    writeLines(renv_entries, renv_file)
    cat('New DATASPACE_CLIENTID set\n')
  }else{
    ## If there is, replace old one
    client_id_entry_index <- which(grepl('DATASPACE_CLIENTID', renv_entries))
    renv_entries[client_id_entry_index] <- paste0('DATASPACE_CLIENTID=', client_id)
    writeLines(renv_entries, renv_file)
    cat('DATASPACE_CLIENTID replaced\n')
  }

  # set key in current session
  Sys.setenv("DATASPACE_CLIENTID" = client_id)

  # Now, let us deal with the client_secret
  ##  If there is no username set, set one
  if(!any(grep('DATASPACE_CLIENTSECRET', renv_entries))){

    renv_entries <- c(renv_entries,
                      paste0('DATASPACE_CLIENTSECRET=', client_secret))
    writeLines(renv_entries, renv_file)
    cat('New DATASPACE_CLIENTSECRET set\n')
  }else{
    ## If there is, replace old one
    client_secret_entry_index <- which(grepl('DATASPACE_CLIENTSECRET', renv_entries))
    renv_entries[client_secret_entry_index] <- paste0('DATASPACE_CLIENTSECRET=', client_secret)
    writeLines(renv_entries, renv_file)
    cat('DATASPACE_CLIENTSECRET replaced\n')
  }

  # set key in current session
  Sys.setenv("DATASPACE_CLIENTSECRET" = client_secret)
}






#'
#' @title Register the Data Space credentials to the R
#'   environment
#' @description Register the Data Space credentials to the R
#'   environment
#' @name pa_initialize_dataspace
#' @rdname pa_initialize_dataspace
#' @param username username used to authenticate the HTTP
#'   request
#' @param password password used to authenticate the HTTP
#'   request
#' @details `pa_initialize_dataspace()` registers the username
#'   and password to the machine's R environment. All the
#'   other functions that rely on authentication will search
#'   for the username and password in the R environment. Do
#'   not share your R environment with others, as they will
#'   be able to read your username and password. You can
#'   register at \url{https://dataspace.copernicus.eu/}.
#' @return NULL
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \dontrun{
#' pa_initialize_dataspace('my-username', 'my-password')
#' }
#'

pa_initialize_dataspace <- function(username, password) {

  ## Path to R env file
  renv.file <- file.path(Sys.getenv("HOME"), ".Renviron")

  ## If the file does not exists, we make one
  if(!file.exists(renv.file)) {
    file.create(renv.file)
  }

  ## Reading entries in the R environment
  renv.entries <- readLines(renv.file)

  # First, we can look for the username

  ##  If there is no username set, set one
  if(!any(grep('DATASPACE_USERNAME', renv.entries))){

    renv.entries <- c(renv.entries,
                      paste0('DATASPACE_USERNAME=', username))
    writeLines(renv.entries, renv.file)
    cat('New DATASPACE_USERNAME set\n')
  }else{
    ## If there is, replace old one
    username.entry.index <- which(grepl('DATASPACE_USERNAME', renv.entries))
    renv.entries[username.entry.index] <- paste0('DATASPACE_USERNAME=', username)
    writeLines(renv.entries, renv.file)
    cat('DATASPACE_USERNAME replaced\n')
  }

  # set key in current session
  Sys.setenv("DATASPACE_USERNAME" = username)

  # Now, let us deal with the password
  ##  If there is no username set, set one
  if(!any(grep('DATASPACE_PASSWORD', renv.entries))){

    renv.entries <- c(renv.entries,
                      paste0('DATASPACE_PASSWORD=', password))
    writeLines(renv.entries, renv.file)
    cat('New DATASPACE_PASSWORD set\n')
  }else{
    ## If there is, replace old one
    password.entry.index <- which(grepl('DATASPACE_PASSWORD', renv.entries))
    renv.entries[password.entry.index] <- paste0('DATASPACE_PASSWORD=', password)
    writeLines(renv.entries, renv.file)
    cat('DATASPACE_PASSWORD replaced\n')
  }

  # set key in current session
  Sys.setenv("DATASPACE_PASSWORD" = password)
}


#'
#' @title Browse satellite products from the Copernicus Data
#'   Space Ecosystem
#' @description Browse satellite products from the
#'   Copernicus Data Space Ecosystem
#' @name pa_browse_dataspace
#' @rdname pa_browse_dataspace
#' @param aoi sf object used to filter satellite products
#' @param start.date beginning of the time window to filter
#' satellite products. The date format should be `\%Y-\%m-\%d`.
#' @param end.date end of the time window to filter
#' satellite products. The date format should be `\%Y-\%m-\%d`.
#' @param max.cloud.cover maximum cloud cover. Values should
#'   be between 0 and 100. Images with cloud cover
#'   assessment greater than this parameter will be removed
#'   from the list.
#' @param collection.name collection of products to filter.
#'   Currently, only SENTINEL-2 is supported.
#' @param product.name partial match of product name used to
#'   filter products. Currently, only supports MSIL2A.
#'   We plan to expand this in the future.
#' @param max.results maximum number of results to return
#' @details `pa_browse_dataspace()` will use HTTP requests to
#'   communicate with the Data Space API and search for
#'   available satellite products matching the filters
#'   established by the function parameters.
#' @return  a list of entries
#'   available for download
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "pacu")
#' area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'), quiet = TRUE)
#' available.images <- pa_browse_dataspace(aoi = area.of.interest,
#'                                         max.cloud.cover = 10,
#'                                         start.date = '2023-01-01',
#'                                         end.date = '2023-12-31')
#' }
#'

pa_browse_dataspace<- function(aoi,
                               start.date,
                               end.date,
                               max.cloud.cover = 100,
                               collection.name = c('SENTINEL-2'),
                               product.name = c('MSIL2A'),
                               max.results = 1000){

  
  url <-  'https://catalogue.dataspace.copernicus.eu/odata/v1/Products'
  req.namespaces <- c('jsonlite', 'httr')
  for (ns in req.namespaces) {
    if(!requireNamespace(ns, quietly = TRUE)){
      warning('The ', ns, ' package is required for this function')
      return(NULL)
    }
  }

  .check.cloud.cover <- as.numeric(max.cloud.cover >= 0) || as.numeric(max.cloud.cover) <= 100
  if(is.na(.check.cloud.cover) || !.check.cloud.cover)
    stop('max.cloud.cover must be a number between 0 and 100')

  if(!inherits(aoi, 'sf'))
    stop('aoi must be an sf object')

  .check.date <- try(as.Date(c(start.date, end.date)), silent = TRUE)
  if(any(is.na(.check.date)) || inherits(.check.date, 'try-error'))
    stop('Dates are not in the correct format. Please enter dates as %Y-%m-%d')


  ## Parsing initial URL
  parsed.url <- httr::parse_url(url)

  ## Creating an empty initial list of filters
  filters <- list()

  ## Creating a filter for collection
  collection.name <- match.arg(collection.name)


  if (!is.null(collection.name)){
    query.collection <- sprintf("Collection/Name eq '%1$s'", collection.name)
    filters <- append(filters, query.collection)
  }


  product.name <- match.arg(product.name)
  query.name <- sprintf("contains(Name,'%1$s')", product.name)
  filters <- append(filters, query.name)

  ## Creating filter for AOI
  aoi <- sf::st_transform(aoi, 4326)
  boundary <- sf::st_bbox(aoi)
  boundary <- sf::st_as_sfc(boundary)
  boundary <- sf::st_cast(boundary, 'POINT')
  boundary <- lapply(boundary, function(x) paste(round(unlist(x), 4), collapse = ' ') )
  boundary <- paste(unlist(boundary), collapse = ',')
  query.area <- sprintf("OData.CSC.Intersects(area=geography'SRID=4326;POLYGON((%1$s))')",
                        boundary)
  filters <- append(filters, query.area)


  ## Creating filter for dates
  start.date <- strftime(start.date,
                         '%Y-%m-%dT00:00:00.000Z')
  end.date <- strftime(end.date,
                       '%Y-%m-%dT23:59:59.999Z')
  query.date <- sprintf("ContentDate/Start gt %1$s and ContentDate/Start lt %2$s",
                        start.date, end.date)
  filters <- append(filters, query.date)

  ## Creating a filter for cloud coverage
  query.cloud <- sprintf( "Attributes/OData.CSC.DoubleAttribute/any(att:att/Name eq 'cloudCover' and att/OData.CSC.DoubleAttribute/Value le %1.2f)",
                          max.cloud.cover)

  filters <- append(filters, query.cloud)

  ## Join all filters into one string
  parsed.url$query$`$filter` <- paste(filters, collapse = ' and ')

  ## Ordering the results
  parsed.url$query$`$orderby` <- 'ContentDate/Start'

  ## Ordering the results
  parsed.url$query$`$top` <- max.results

  ## Build the url to send the request
  query.url <- httr::build_url(parsed.url)

  ## Send the request
  resp <- httr::GET(query.url)

  ## Parse the json response
  rj <- jsonlite::fromJSON(httr::content(resp, 'text', encoding = 'UTF-8'))

  ## Convert to data frame
  results <- as.data.frame(rj$value)

  attr(results, 'start.date') <- start.date
  attr(results, 'end.date') <- end.date
  attr(results, 'max.cloud.cover') <- max.cloud.cover
  attr(results, 'collection.name') <- collection.name
  attr(results, 'product.name') <- product.name

  class(results) <- c('dslist', class(results))
  return(results)

}

#'
#' @title Download satellite products from the Copernicus
#'   Data Space Ecosystem
#' @description Download satellite products from the
#'   Copernicus Data Space Ecosystem to find satellite
#'   products
#' @name pa_download_dataspace
#' @rdname pa_download_dataspace
#' @param x object of class \sQuote{dslist}
#' @param dir.path directory path to which the files will be
#'   saved
#' @param aoi NULL or an sf object. If an area of interest
#'   (aoi) is provided, the downloaded zip files will be
#'   cropped to the aoi. This was designed to save storage
#'   space.
#' @details `pa_download_dataspace()` uses the object from
#'   `pa_browse_dataspace()` to download the data from
#'   Copernicus Data Space. The aoi argument is optional but
#'   was designed to save storage space.
#' @return a list of objects
#'   that could not be downloaded
#' @author Caio dos Santos and Fernando Miguez
#' @export
#' @examples
#' \dontrun{
#' extd.dir <- system.file("extdata", package = "pacu")
#' area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'), quiet = TRUE)
#' available.images <- pa_browse_dataspace(aoi = area.of.interest,
#'                                         max.cloud.cover = 10,
#'                                         start.date = '2023-01-01',
#'                                         end.date = '2023-12-31')
#' dwonloaded.images <- pa_download_dataspace(x = available.images)
#'
#' }
#'

pa_download_dataspace <- function(x,
                                  dir.path = NULL,
                                  aoi = NULL,
                                  verbose = TRUE) {

  ## Checking for the right format in the x object
  if(!inherits(x, 'dslist'))
    stop('x must be a data frame generated with pa_browse_dataspace')

  ## Checking for credentials
  if("" %in% c(Sys.getenv('DATASPACE_USERNAME'), Sys.getenv('DATASPACE_PASSWORD')))
    stop('Dataspace password or username not registered in R envinronemt. Use initialize_dataspace to register credentials to R environment.')

  # Checking dependencies
  req.namespaces <- c('jsonlite', 'httr')
  for (ns in req.namespaces) {
    if(!requireNamespace(ns, quietly = TRUE)){
      warning('The ', ns, ' package is required for this function')
      return(NULL)
    }
  }

  if(verbose == 1){
    progress.bar <- utils::txtProgressBar(min = 0, 
                                          max = nrow(x),
                                          style = 3,
                                          initial = 0)
    on.exit(close(progress.bar))
  }

  if(is.null(dir.path))
    dir.path <- '.'

  if(!is.null(dir.path)){
    if (!dir.exists(dir.path))
      stop('The path provided to dir.path does not exist')
  }

  username <- Sys.getenv('DATASPACE_USERNAME')
  password <- Sys.getenv('DATASPACE_PASSWORD')
  ## Get initial token
  token <- .pa_get_dataspace_token(username, password)

  ## Vector for successfully downloaded
  sccs <- c()
  ## Vector of failed downloads
  fails <- c()
  ## Go down the list of images to be downloaded
  for (i in 1:length(x$Id)){

    outpath <- file.path(dir.path, gsub('.SAFE', '.zip', x$Name[i]))

    ## Checking for files that have been downloaded previously
    if (file.exists(outpath)) {
      cat('File ', basename(outpath), ' has been downloaded. Moving to next.\n')
      next
    }

    ## Check for expired token
    if(Sys.time() > token$timestamp + 0.9 * (token$expires_in))
      token <- .pa_get_dataspace_token(username, password)


    url <- sprintf("https://catalogue.dataspace.copernicus.eu/odata/v1/Products(%1$s)/$value",
                   x$Id[i])

    if (verbose > 1){
    cat('Downloading ', x$Name[i], '\n')
    }

    resp <- httr::GET(url,
                      httr::add_headers(Authorization = paste("Bearer", token$access_token, sep = " ")),
                      httr::config(followlocation = FALSE))

    while (httr::status_code(resp) %in% c(301, 302, 303, 307)) {

      url <- httr::headers(resp)$location
      resp <- try(httr::RETRY('GET',url,
                              httr::add_headers(Authorization = paste("Bearer", token$access_token, sep = " ")),
                              httr::write_disk(outpath, overwrite = TRUE),
                              httr::config(followlocation = FALSE)),
                  silent = TRUE)
    }
    ## Checking if the download worked
    if(inherits(resp, 'try-error') || try(httr::status_code(resp), silent = TRUE) != 200){
      file.remove(outpath)
      fails <- c(fails, i)
      cat('There was an error downloading this file \n')
    }else{
      if (!is.null(aoi))
        .pa_crop_s2_to_aoi(outpath, aoi)
    }

    if( verbose == 1){
      utils::setTxtProgressBar(progress.bar, utils::getTxtProgressBar(progress.bar) + 1) 
    }
    sccs <- c(sccs, outpath)
  }
  return(sccs)
}

pkgname <- "pacu"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('pacu')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("pa_2utm")
### * pa_2utm

flush(stderr()); flush(stdout())

### Name: pa_2utm
### Title: Reproject a sf object to UTM coordinates
### Aliases: pa_2utm

### ** Examples





cleanEx()
nameEx("pa_apportion_mass")
### * pa_apportion_mass

flush(stderr()); flush(stdout())

### Name: pa_apportion_mass
### Title: Impose a regular grid over yield polygons
### Aliases: pa_apportion_mass

### ** Examples





cleanEx()
nameEx("pa_browse_dataspace")
### * pa_browse_dataspace

flush(stderr()); flush(stdout())

### Name: pa_browse_dataspace
### Title: Browse Copernicus Data Space products
### Aliases: pa_browse_dataspace

### ** Examples

## Not run: 
##D extd.dir <- system.file("extdata", package = "pacu")
##D area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'), quiet = TRUE)
##D available.images <- pa_browse_dataspace(aoi = area.of.interest,
##D                                         max.cloud.cover = 10,
##D                                         start.date = '2023-01-01',
##D                                         end.date = '2023-12-31')
## End(Not run)




cleanEx()
nameEx("pa_cardinal_dates")
### * pa_cardinal_dates

flush(stderr()); flush(stdout())

### Name: pa_cardinal_dates
### Title: Predict cardinal dates from satellite image data
### Aliases: pa_cardinal_dates pa_cardinal_dates.numeric
###   pa_cardinal_dates.Date pa_cardinal_dates.veg.index

### ** Examples

## Not run: 
##D x <- seq(1, 365, 6)
##D y <- nlraa::scard3(x, 120, 210, 300)
##D pa_cardinal_dates.vector(
##D   x = x,
##D   y = y,
##D   model = 'scard3',
##D   prior.means = c(130, 190, 297),
##D   prior.vars = c(11, 13, 18),
##D   bias.correction = c(10, 10, 10)
##D )
## End(Not run)




cleanEx()
nameEx("pa_check_yield")
### * pa_check_yield

flush(stderr()); flush(stdout())

### Name: pa_check_yield
### Title: Check yield data before processing with pa_yield
### Aliases: pa_check_yield

### ** Examples





cleanEx()
nameEx("pa_compute_vi")
### * pa_compute_vi

flush(stderr()); flush(stdout())

### Name: pa_compute_vi
### Title: Compute vegetation indices from a zipped Sentinel 2 file
### Aliases: pa_compute_vi

### ** Examples





cleanEx()
nameEx("pa_download_dataspace")
### * pa_download_dataspace

flush(stderr()); flush(stdout())

### Name: pa_download_dataspace
### Title: Download Copernicus Data Space products
### Aliases: pa_download_dataspace

### ** Examples

## Not run: 
##D extd.dir <- system.file("extdata", package = "pacu")
##D area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'), quiet = TRUE)
##D available.images <- pa_browse_dataspace(aoi = area.of.interest,
##D                                         max.cloud.cover = 10,
##D                                         start.date = '2023-01-01',
##D                                         end.date = '2023-12-31')
##D ## download only one image for a quick test
##D downloaded.images <- pa_download_dataspace(x = available.images[1, ],
##D                                            dir.path = tempdir(),
##D                                            aoi = area.of.interest)
## End(Not run)




cleanEx()
nameEx("pa_get_rgb")
### * pa_get_rgb

flush(stderr()); flush(stdout())

### Name: pa_get_rgb
### Title: Retrieve an RGB image from a zipped Sentinel 2 file
### Aliases: pa_get_rgb

### ** Examples





cleanEx()
nameEx("pa_get_vi_stats")
### * pa_get_vi_stats

flush(stderr()); flush(stdout())

### Name: pa_get_vi_stats
### Title: Request vegetation index statistics from the Data Space
###   Statistics API
### Aliases: pa_get_vi_stats

### ** Examples

## Not run: 
##D extd.dir <- system.file("extdata", package = "pacu")
##D area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'), quiet = TRUE)
##D ndvi <- pa_get_vi_stats(aoi = area.of.interest,
##D                         start.date = '2021-01-01',
##D                         end.date = '2021-12-31',
##D                         vegetation.index = 'ndvi')
## End(Not run)




cleanEx()
nameEx("pa_initialize_dataspace")
### * pa_initialize_dataspace

flush(stderr()); flush(stdout())

### Name: pa_initialize_dataspace
### Title: Register Copernicus Data Space credentials
### Aliases: pa_initialize_dataspace

### ** Examples

## Not run: 
##D pa_initialize_dataspace('my-username', 'my-password')
## End(Not run)




cleanEx()
nameEx("pa_initialize_oauth")
### * pa_initialize_oauth

flush(stderr()); flush(stdout())

### Name: pa_initialize_oauth
### Title: Register OAuth 2.0 credentials for the Statistics API
### Aliases: pa_initialize_oauth

### ** Examples

## Not run: 
##D pa_initialize_oauth('my-client-id', 'my-client-secret')
## End(Not run)




cleanEx()
nameEx("pa_make_vehicle_polygons")
### * pa_make_vehicle_polygons

flush(stderr()); flush(stdout())

### Name: pa_make_vehicle_polygons
### Title: Make vehicular polygons for yield monitor observations
### Aliases: pa_make_vehicle_polygons

### ** Examples





cleanEx()
nameEx("pa_plot")
### * pa_plot

flush(stderr()); flush(stdout())

### Name: pa_plot
### Title: Create a plot from a pacu object
### Aliases: pa_plot pa_plot.yield pa_plot.trial pa_plot.veg.index
###   pa_plot.rgb pa_plot.met

### ** Examples

## Not run: 
##D ## for examples, please see the pacu vignette
## End(Not run)




cleanEx()
nameEx("pa_trial")
### * pa_trial

flush(stderr()); flush(stdout())

### Name: pa_trial
### Title: EXPERIMENTAL FUNCTION - Create an interpolated trial object from
###   as-applied data
### Aliases: pa_trial

### ** Examples

## Not run: 
##D ## tbd
## End(Not run)




cleanEx()
nameEx("pa_weather_summary")
### * pa_weather_summary

flush(stderr()); flush(stdout())

### Name: pa_get_weather_sf
### Title: Retrieve weather data as a met object
### Aliases: pa_get_weather_sf

### ** Examples

## Not run: 
##D extd.dir <- system.file("extdata", package = "pacu")
##D area.of.interest <- sf::st_read(file.path(extd.dir, 'cobs_a_aoi.shp'), quiet = TRUE)
##D weather.met <- pa_get_weather_sf(aoi = area.of.interest,
##D                                  start.date = '1990-01-01',
##D                                  end.date = '2020-12-31',
##D                                  source = 'power')
##D summary(weather.met)
## End(Not run)





cleanEx()
nameEx("pa_yield")
### * pa_yield

flush(stderr()); flush(stdout())

### Name: pa_yield
### Title: Create an interpolated yield object from raw data
### Aliases: pa_yield

### ** Examples

## Not run: 
##D extd.dir <- system.file("extdata", package = "pacu")
##D raw.yield <- sf::read_sf(file.path(extd.dir, '2012-basswood.shp'),
##D                          quiet = TRUE)
##D boundary <- sf::read_sf(file.path(extd.dir, 'boundary.shp'),
##D                         quiet = TRUE)
##D ## the simple algorithm
##D ymp.simple <- pa_yield(input = raw.yield,
##D                        boundary = boundary,
##D                        algorithm = 'simple',
##D                        unit.system = 'metric',
##D                        lbs.per.bushel = 56,
##D                        verbose = FALSE) ## 56 lb/bushel for maize
##D ymp.simple
##D 
##D ## the ritas algorithm
##D ymp.ritas <- pa_yield(input = raw.yield,
##D                       boundary = boundary,
##D                       algorithm = 'ritas',
##D                       unit.system = 'metric',
##D                       lbs.per.bushel = 56,
##D                       verbose = FALSE)
##D ymp.ritas
## End(Not run)




cleanEx()
nameEx("pacu.options")
### * pacu.options

flush(stderr()); flush(stdout())

### Name: pacu.options
### Title: Environment which stores PACU options
### Aliases: pacu.options
### Keywords: datasets

### ** Examples





cleanEx()
nameEx("pacu_options")
### * pacu_options

flush(stderr()); flush(stdout())

### Name: pacu_options
### Title: Set pacu options
### Aliases: pacu_options

### ** Examples




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

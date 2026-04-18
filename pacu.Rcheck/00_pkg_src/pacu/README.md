# pacu: Precision Agriculture Computational Utilities <img src="man/figures/logo.png" align="right" height="120" alt="" />

[![CRAN](http://www.r-pkg.org/badges/version/pacu)](https://CRAN.R-project.org/package=pacu)
[![CRAN
downloads total](https://cranlogs.r-pkg.org/badges/grand-total/pacu)](https://github.com/r-hub/cranlogs.app)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/pacu)](https://cran.r-project.org/package=pacu)

The *pacu* package supports common precision agriculture workflows in R. It includes tools to process, visualize, and analyze yield monitor data, retrieve and summarize weather data, and download or summarize Sentinel-2 satellite imagery.


## Installation

pacu is available on CRAN:

```r
install.packages("pacu")
```


To install the development version from GitHub, use either `devtools` or `remotes`:

```r
devtools::install_github("cldossantos/pacu")
library(pacu)
```

```r
remotes::install_github("cldossantos/pacu")
library(pacu)
```

Vignettes are not built automatically when the package is installed from GitHub. If you want the full tutorials, install with vignette building enabled:

```r
devtools::install_github("cldossantos/pacu", build_vignettes = TRUE)
browseVignettes(package = "pacu")
```

```r
remotes::install_github("cldossantos/pacu", build_vignettes = TRUE)
browseVignettes(package = "pacu")
```


## Package requirements

Core imports: stars, XML, gstat, units, sf, apsimx, tmap, httr, jsonlite

Suggested packages for examples, visualization, and development: spData, knitr, mgcv, concaveman, rmarkdown, ggplot2, patchwork, nasapower, testthat

## Manuscript

The manuscript describing *pacu* is available [here](https://doi.org/10.1016/j.softx.2024.101971).

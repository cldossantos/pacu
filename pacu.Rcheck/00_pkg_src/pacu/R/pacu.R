#' Set pacu options
#'
#' @title Set pacu options
#' @name pacu_options
#' @description Set package options that control messages and a few default
#'   behaviors.
#' @param suppress.warnings logical; whether to suppress warning messages.
#' @param suppress.messages logical; whether to suppress informative messages.
#' @param apportion.size.multiplier numeric multiplier used to determine the size
#'   of the apportioning polygons in the RITAS algorithm. A value of `sqrt(2)`
#'   makes polygons approximately the same size as the harvest polygons. Smaller
#'   values increase resolution but can substantially increase computation time.
#' @param minimum.coverage.fraction minimum fraction of an apportioning polygon
#'   that must be covered before the apportioning step is performed.
#' @return Called for side effects; modifies the \sQuote{pacu.options}
#'   environment.
#' @export
#' @examples
#' \donttest{
#' names(pacu.options)
#' pacu_options(suppress.warnings = TRUE, suppress.messages = TRUE)
#' pacu.options$suppress.warnings
#' pacu.options$suppress.messages
#' }

pacu_options <- function(suppress.warnings = FALSE, 
                         suppress.messages = FALSE,
                         apportion.size.multiplier = 1,
                         minimum.coverage.fraction = 0.5){

  assign('suppress.warnings', suppress.warnings, pacu.options)
  assign('suppress.messages', suppress.messages, pacu.options)
  assign('apportion.size.multiplier', apportion.size.multiplier, pacu.options)
  assign('minimum.coverage.fraction', minimum.coverage.fraction, pacu.options)
}

#' Environment which stores PACU options
#'
#' @title Environment which stores PACU options
#' @name pacu.options
#' @description Environment used to store package options such as warning and
#'   message settings. Using an environment avoids relying on global variables or
#'   other patterns with undesirable side effects.
#' @return This is an environment, not a function, so nothing is returned.
#' @export
#' @examples
#' \donttest{
#' names(pacu.options)
#' ## to suppress messages
#' pacu_options(suppress.messages = TRUE)
#' }
#'

pacu.options <- new.env(parent = emptyenv())
assign('suppress.warnings', FALSE, pacu.options)
assign('suppress.messages', FALSE, pacu.options)
assign('apportion.size.multiplier', 1, pacu.options)
assign('.run.local.tests', TRUE, pacu.options)
assign('.run.experimental.tests', FALSE, pacu.options)
assign('minimum.coverage.fraction', 0.50, pacu.options)


## Import packages needed for pacu to work correctly
#' @import apsimx gstat httr jsonlite sf stars units XML
#' @importFrom grDevices hcl.colors
## @importFrom utils read.table write.table packageVersion zip unzip
## @importFrom tools file_path_sans_ext file_ext
## @importFrom stats aggregate anova coef cor cov2cor deviance lm optim qt var sd setNames sigma integrate median na.omit weighted.mean
## @importFrom graphics legend, par, points
NULL

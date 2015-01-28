#' tradeflows: Manipulate data for a forest products trade flow database
#'
#' This package prepares data for a forest products trade flow database.
#' It contains functions that automatically grab data from the
#' COMTRADE and COMEXT databases, feed it through algorithms to compare
#' unit values, to mirror export and import values.
#' Data will be delivered to an online user-interface.
#' \itemize{
#'  \item{\code{\link{clean}()}}{implements most of the
#' data cleaning mechanism by calling other functions
#' that estimate a trade flow quantity.}
#'  \item{\code{\link{cleandbproduct}()}}{loads data
#'  from a database table, cleans with the clean() function
#'  and write data to a database}
#'}
#' To learn more about the tradeflows package,
#' start with the vignettes:
#' \code{browseVignettes(package = "tradeflows")}
#'
#' @docType package
#' @name tradeflows
#' @import dplyr
NULL

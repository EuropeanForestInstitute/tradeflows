#' Write errors to a log file
#' @param condition warning or error condition as returned by \link{tryCatch}
#' @param logfile character path to a log file
#' @param optionaltext character additional text added after the timestamp
#' @examples
#' # Create a log file name
#' logfileexample <- file.path(tempdir(),"log.txt")
#' # Catch an error
#'  tryCatch({
#'      stop("There is an error in this exampe.")
#'  }, error = function(errorcondition){
#'             write2log(errorcondition, logfileexample, "code: 123456")
#'  })
#'  # Read the log file
#' readLines(logfileexample)
#' @export
write2log <- function(condition, logfile, optionaltext = NULL){
    write(paste(as.character(Sys.time()), optionaltext, "\n",
                toString(condition)),
          logfile, append=TRUE)
}



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

#' Add a dot at the end of a text file
#' while keeping a new line character at the end of the file
#' so that it is valid for \code{readLines}.
#' @param logfile character path to a log file
#' @export
adddot2logfile <- function(logfile){
    if(file.exists(logfile)){
        logfilecontent <- readLines(logfile)
    } else {
        logfilecontent <- ""
    }
    # Add a dot to the last line
    logfilecontent[length(logfilecontent)] <- paste0(
        logfilecontent[length(logfilecontent)],
        ".")
    # Write back to the file
    writeLines(logfilecontent, logfile)
}


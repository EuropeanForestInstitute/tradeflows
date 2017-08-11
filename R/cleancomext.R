#' Extract a list of unique, most recent product and country codes from Comext
#'
#' Takes a raw codes table from comext, select codes which have the most
#' recent \code{datestart} and make sure they are unique.
#' @param RMySQLcon database connection object created by \code{RMySQL::\link{dbConnect}}
#' @param tableread character name of the table to read from
#' @param tablewrite character name of the table to write to
#' @param codevariable unquoted code variable (à la dplyr verbs)
#' @return TRUE on success
#' The output is actually a database table containing the cleaned codes.
#' @examples \dontrun{
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
#' # Write dummy codes to the database
#' raw_code <- data.frame(code = c(4L, 4L), datestart = c(1L, 2L))
#' RMySQL::dbWriteTable(con, "raw_code", raw_code, row.names = FALSE, overwrite = TRUE)
#' # Clean them
#' cleancode(con, tableread = "raw_code", tablewrite = "val_code", codevariable = "code")
#' # Clean real comext codes
#'
#'
#' on.exit(RMySQL::dbDisconnect(con))
#' }
#' @export
cleancode <- function(RMySQLcon, tableread, tablewrite, codevariable){
    require(dplyr)
    # Implementation based on the "programming with dplyr" vignette
    # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
    codevariable <- enquo(codevariable)
    # Load data and keep only most recent codes
    dtf <- tbl(RMySQLcon, tableread) %>%
        collect() %>%
        group_by(!!codevariable) %>%
        filter(datestart == max(datestart))
    # The number of distinct rows for all columns should be equal to
    # the number of distinct codes
    stopifnot(identical(nrow(unique(dtf)),
                        nrow(distinct(dtf, !!codevariable))))
    # Remove duplicates
    dtf <- unique(dtf)
    # Write back to the database
    RMySQL::dbWriteTable(RMySQLcon, tablewrite, dtf, row.names = FALSE, overwrite = TRUE)
}

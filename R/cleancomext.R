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
#' @examples \dontrun{ # Clean product and country codes
#' # Connect to the database
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
#' # Write dummy codes to the database table "raw_code"
#' raw_code <- data.frame(code = c(4L, 4L), datestart = c(1L, 2L))
#' RMySQL::dbWriteTable(con, "raw_code", raw_code, row.names = FALSE, overwrite = TRUE)
#' # Clean the codes and write them to the database table "val_code" (for validated code)
#' cleancode(con, tableread = "raw_code", tablewrite = "val_code", codevariable = "code")
#'
#' # Comext codes
#' if(FALSE){ # If raw codes are not present, transfer them
#' tradeharvester::transfertxtcodesfolder2db(con, rawdatacomextfolder = "~/R/tradeharvester/data-raw/comext/201707/text/english/")
#' }
#' # Clean comext product, reporter and partner codes
#' cleanallcomextcodes(con)
#' # Disconnect from the database
#' RMySQL::dbDisconnect(con)
#' }
#' @export
cleancode <- function(RMySQLcon, tableread, tablewrite, codevariable){
    require(dplyr)
    # Implementation based on the "programming with dplyr" vignette
    # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
    codevariable <- enquo(codevariable)

    # Check if output fields are in input fields
    inputfields <- RMySQL::dbListFields(RMySQLcon, tableread)
    outputfields <- RMySQL::dbListFields(RMySQLcon, tablewrite)
    stopifnot(outputfields %in% inputfields)

    # This function cannot use  RMySQL::dbWriteTable with overwrite = TRUE
    # because this would also overwrites the field types and indexes.
    # dbWriteTable chooses default types that are not optimal,
    # for example, it changes date fields to text fields.
    # Therefore use RMySQL::dbWriteTable with append = TRUE,
    # but first check if the table is empty
    # and if it is not empty, ask to recreate the database
    # structure with empty tables.
    res <- RMySQL::dbSendQuery(RMySQLcon, sprintf("SELECT COUNT(*) as nrow FROM %s;",tablewrite))
    sqltable <- RMySQL::dbFetch(res)
    RMySQL::dbClearResult(res)
    # Check if the output table is empty
    if(sqltable$nrow > 0){
        stop("Table ", tablewrite, " is not empty.",
             "You can recreate an empty table structure with:\n",
             sprintf("tradeflows::createdbstructure(sqlfile = 'val_comext.sql', dbname = '%s')",
                     RMySQL::dbGetInfo(RMySQLcon)$dbname))
    }


    # load all codes and keep only most recent codes
    dtf <- tbl(RMySQLcon, tableread) %>%
        collect() %>%
        group_by(!!codevariable) %>%
        filter(datestart == max(datestart)) %>%
        select(outputfields)
    # The number of distinct rows for all columns should be equal to
    # the number of distinct codes
    stopifnot(identical(nrow(unique(dtf)),
                        nrow(distinct(dtf, !!codevariable))))
    # Remove duplicates
    dtf <- unique(dtf)
    # Write back to the database
    RMySQL::dbWriteTable(RMySQLcon, tablewrite, dtf,
                         row.names = FALSE, append = TRUE)
}



#' @description \code{cleanallcomextcodes} extracts unique product
#' and country codes from the Comext raw data so that they are ready for use
#' as unique keys.
#' It is a porcelaine function based on the plumbing function \code{cleancode}.
#'
#' @rdname cleancode
#' @export
cleanallcomextcodes <- function(RMySQLcon){
    createdbstructure(sqlfile = "val_comext.sql",
                      # extract db name from the RMySQL connection object
                      dbname = RMySQL::dbGetInfo(RMySQLcon)$dbname)
    message("Cleaning product, reporter and partner codes...")
    cleancode(RMySQLcon, "raw_comext_product", "val_comext_product", productcode)
    cleancode(RMySQLcon, "raw_comext_reporter", "val_comext_reporter", reportercode)
    cleancode(RMySQLcon, "raw_comext_partner", "val_comext_partner", partnercode)

    # Diagnostics
    # Display row count information
    # based on https://stackoverflow.com/a/1775272/2641825
    res <- RMySQL::dbSendQuery(RMySQLcon, "SELECT
    (SELECT COUNT(*) FROM   val_comext_product)  AS product,
    (SELECT COUNT(*) FROM   val_comext_reporter) AS reporter,
    (SELECT COUNT(*) FROM   val_comext_partner)  AS partner")
    nrows <- RMySQL::dbFetch(res)
    RMySQL::dbClearResult(res)
    message("Transfered:\n",
            nrows$product, " rows to the val_comext_product table\n",
            nrows$reporter, " rows to the val_comext_reporter table\n",
            nrows$partner, " rows to the val_comext_partner table.\n")
}

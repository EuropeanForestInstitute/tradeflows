#' Sets database connection parameters as a global R option tradeflowsDB
#'
#' Database connection parameters
#' Are storred in the installed version of the package
#' Run this command to find the location of the configuration file on your machine:
#' system.file("config/databaseconf.R", package="tradeflows").
#' Use reload=TRUE to force reloading the file after modification.
#' @param reload logical, force reloading the configuration file
#' @param silent logical, do not print "already loaded" message when TRUE
#' @export
setdatabaseconfig <- function(reload = FALSE, silent = FALSE){
    if(is.null(getOption("tradeflowsDB"))|reload){
        # Path to the databasesonfig.R file
        databaseconfig <- system.file("config/databaseconfig.R",
                                      package="tradeflows")
        message(paste("Loading database configuration from ",
                      databaseconfig))
        source(databaseconfig)
    } else {
        if (!silent){
            message("Database configuration file already loaded.")
            message("Use the option reload=TRUE if you want to reload it.")
        }
    }
}


#' Control the presence of required columns in the
#' given database tables
#'
#' Database connection parameters are given by the global option
#' getOption("tradeflowsDB").
#' @param tables character vector containing the name of
#' database tables to check
#' @export
checkdbcolumns <- function(tables = c("raw_flow_yearly", "validated_flow_yearly")){
    require(dplyr)
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBread <- RMySQL::dbConnect(RMySQL::MySQL(), user=db["user"], host=db["host"],
                         password=db["password"], dbname=db["dbname"])
    for(tableread in tables){
        print(str(tableread))
        if(!tableread %in% names(column_names)){
            stop("Add ", tableread ," to the column_names table")
        }
        sqlquery <- paste(c("SELECT `COLUMN_NAME`",
                            "FROM `INFORMATION_SCHEMA`.`COLUMNS` ",
                            "WHERE `TABLE_SCHEMA`='tradeflows' ",
                            "AND `TABLE_NAME`='",tableread,"';"), collapse = "")
        res <- RMySQL::dbSendQuery(DBread, sqlquery)
        columnname <- RMySQL::dbFetch(res)
        columnname <- columnname$COLUMN_NAME
        # For that table, find the efi column names which are supposed to
        # be in there, but which are not in the data frame
        missingcolumns <- column_names$efi[column_names[c(tableread)] &
                                               !column_names$efi %in% columnname]
        if (length(missingcolumns)>0){
            warning("Following column_names are missing from the ",
                    tableread," table: \n",
                    paste(missingcolumns, collapse=", "))
        }
    }
}


#' Load trade flows for one product from a database table of choice
#'
#' Connect to the raw database and load all tradeflows
#' for the given product code,
#' for all years, in all directions, for all reporter and all partners.
#' @param productcode_ the code of a product
#' @param tableread the database table to read from
#' @param convcontrynames logical weather to convert contry names
#' @examples
#'\dontrun{
#' othersawnwod <- readdbproduct(440799, "raw_flow_yearly")
#' head(othersawnwod[c("year","reporter","partner","flow","tradevalue","quantity","weight")])
#' }
#' @export
readdbproduct <- function(productcode_, tableread, convcountrynames = FALSE){
    require(dplyr)
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    rawdata <- tbl(DBread, tableread)
    dtf <- rawdata %>%
        filter(productcode == productcode_) %>%
        # keep id in order to compare raw and cleaned trade flows
        # forces computation and brings data back into a data.frame
        collect  %>%
        # Change year to an integer
        mutate(year = as.integer(year))
    if (convcountrynames){
        dtf <- dtf %>% # Convert country names to utf-8
                mutate(reporter = iconv(reporter, "latin1", "utf-8"),
                   partner = iconv(partner, "latin1", "utf-8"))
        message("Changed reporter and partner encoding from latin1 to utf8.",
                "(check the reporter code 384 côte d'ivoire)")
    }
    # Comment out this check which might break for unnecessary reasons
    # if EFI developers decide to add extra columns in the database
    # stopifnot(names(dtf) %in% column_names$efi)
    return(dtf)
}


#' Write validated flows for one product into the database
#'
#' Write data into the DB
#' @param dtf a data frame containing the data to be inserted
#' @param tablewrite character vector of a database table to write to
#' @export
writedbproduct <- function(dtf, tablewrite){
    # Write only to a validated_flow table
    stopifnot(tablewrite %in% c("validated_flow_yearly", "validated_flow_monthly"))
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBwrite <- RMySQL::dbConnect(RMySQL::MySQL(),
                                 user=db["user"], host=db["host"],
                                 password=db["password"], dbname=db["dbname"])
    dtf <- data.frame(dtf)
    result <- RMySQL::dbWriteTable(DBwrite, name = tablewrite,
                           value=dtf, append=TRUE, row.names = FALSE)
    RMySQL::dbDisconnect(DBwrite)
    return(result)
}


#' Delete validated data for one product
#'@param product name
#'@export
deletedbproduct <- function(productcode, tabledelete){
    # Delete only in a validated_flow table
    stopifnot(tabledelete %in% c("validated_flow_yearly", "validated_flow_monthly"))
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBwrite <- RMySQL::dbConnect(RMySQL::MySQL(),
                                 user=db["user"], host=db["host"],
                                 password=db["password"], dbname=db["dbname"])
    sqlproduct <- paste("DELETE FROM ",tabledelete,
                         "WHERE productcode = ", productcode)
    res <- RMySQL::dbSendQuery(DBwrite, sqlproduct)
    nbrowsdeleted <- RMySQL::dbGetRowsAffected(res)
    message(nbrowsdeleted," rows were deleted.")
    RMySQL::dbClearResult(res)
    return(nbrowsdeleted)
}


#' Number of rows in a database table
#'
#' @param table name of a table
#' @export
nrowinDB <- function(tableread){
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    tabledata <- tbl(DBread, tableread)
    tabledata %>% summarise(nrow = n()) %>% collect
}


#' Create a dplyr connector to MySQL
#'
#' Return a dplyr tbl object for the given MySQL database table
#' tbl objects allow lazy operations. See src_mysql.
#'
#' @param tableread name of the database table to read
#' @return a dplyr tbl object
#' @examples
#'\dontrun{
#' readdbtbl("raw_flow_yearly") %>%
#'     group_by(productcode) %>%
#'     summarise(nrow = n()) %>%
#'     arrange(nrow) %>%
#'     kable
#' }
#' @export
readdbtbl <- function(tableread){
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    return(tbl(DBread, tableread))
}


#' Select rows by id
#'
#' @param table name of a table
#' @param id a vector of id
#' @export
selectbyid <- function(tableread, id){
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    tabledata <- tbl(DBread, tableread)
    tabledata %>% summarise(nrow = n()) %>% collect
}


if (FALSE){
    #fibre <- loadrawdata(550410)
#     charcoal90 <- readdb(440290)
    swd99 <- readdb(440799, tableread = "raw_flow_yearly" )
# See clean.R script for write example
}

#' Sets database connection parameters as a global R option tradeflowsDB
#'
#' Database connection parameters
#' Are storred in the installed version of the package
#' Run this command to find the location of the configuration file on your machine:
#' system.file("config/databaseconf.R", package="tradeflows").
#' Use reload=TRUE to force reloading the file after modification.
#' @param reload logical
#' @export
setdatabaseconfig <- function(reload=FALSE, message=TRUE){
    # Path to the databasesonfig.R file
    databaseconfig <- system.file("config/databaseconfig.R",
                                  package="tradeflows")
    if(is.null(getOption("tradeflowsDB"))|reload){
        message(paste("Loading database configuration from ",
                      databaseconfig))
        source(databaseconfig)
    } else {
        if (message){
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
    setdatabaseconfig(message=FALSE)
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
        res <- dbSendQuery(DBread, sqlquery)
        columnname <- dbFetch(res)
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
#' @export
readdbproduct <- function(productcode_, tableread ){
    require(dplyr)
    setdatabaseconfig(message=FALSE)
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    rawdata <- tbl(DBread, tableread)
    dtf <- rawdata %>% filter(productcode == productcode_) %>%
        # Remove id as it is database specific and should not be carried through cleaning
        # Change this to remove all fields that are not part of column_names$efi
        select(-id, -lastchanged) %>%
        collect  %>% # forces computation and brings data back into a data.frame
        mutate(year = as.integer(year)) # Change year to an integer
    # Comment out this check which might not be needed
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
    setdatabaseconfig(message=FALSE)
    require(RMySQL)
    db <- getOption("tradeflowsDB")
    DBwrite <- dbConnect(MySQL(), user=db["user"], host=db["host"],
                         password=db["password"], dbname=db["dbname"])
    dtf <- data.frame(dtf)
    result <- dbWriteTable(DBwrite, name = tablewrite,
                           value=dtf, append=TRUE, row.names = FALSE)
    dbDisconnect(DBwrite)
    return(result)
}


#' Delete validated data for one product
#'@param product name
#'@export
deletedbproduct <- function(productcode, tabledelete){
    # Delete only in a validated_flow table
    require(RMySQL)
    stopifnot(tabledelete %in% c("validated_flow_yearly", "validated_flow_monthly"))
    setdatabaseconfig(message=FALSE)
    db <- getOption("tradeflowsDB")
    DBwrite <- dbConnect(MySQL(), user=db["user"], host=db["host"],
                         password=db["password"], dbname=db["dbname"])
    sqlproduct <- paste("DELETE FROM ",tabledelete,
                         "WHERE productcode = ", productcode)
    res <- dbSendQuery(DBwrite, sqlproduct)
    # See ?dbSendQuery
    nbrowsdeleted <- dbGetRowsAffected(res)
    message(nbrowsdeleted," rows were deleted.")
    dbClearResult(res)
    return(nbrowsdeleted)
}


#' Number of rows in a database table
#'
#' @param table name of a table
nrowinDB <- function(table){
    DBread <- src_mysql(user="tradeflows", host="localhost",
                        password="tradeflows", dbname="tradeflows")
    rawdata <- tbl(DBread, sql(paste("SELECT * FROM",table)))
    rawdata %>% summarise(nrow = n()) %>% collect
}


if (FALSE){
    #fibre <- loadrawdata(550410)
#     charcoal90 <- readdb(440290)
    swd99 <- readdb(440799, tableread = "raw_flow_yearly" )
# See clean.R script for write example
}

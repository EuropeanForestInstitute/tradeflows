#' Sets database connection parameters as a global R option tradeflowsDB
#'
#' Database connection parameters
#' Are storred in the installed version of the package
#' Run this command to find the location of the configuration file on your machine:
#' system.file("config/databaseconf.R", package="tradeflows").
#' Use reload=TRUE to force reloading the file after modification.
#' @param reload logical
#' @export
setdatabaseconfig <- function(reload=FALSE){
    # Path to the databasesonfig.R file
    databaseconfig <- system.file("config/databaseconfig.R",
                                  package="tradeflows")
    if(is.null(getOption("tradeflowsDB"))|reload){
        message(paste("Loading database configuration from ",
                      databaseconfig))
        source(databaseconfig)
    } else {
        message("Database configuration file already loaded.")
        message("Use the option reload=TRUE if you want to reload it.")
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
checkdbcolumns <- function(tables = c("raw_flow", "validated_flow")){
    require(dplyr)
    if(is.null(getOption("tradeflowsDB"))){
        setdatabaseconfig()
    }
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    for(table in tables){
        rawdata <- tbl(DBread, table)
        dtf <- rawdata %>% head
        missingcolumns <- column_names$efi[column_names[c(table)] &
                                               !column_names$efi %in% names(dtf)]
        if (length(missingcolumns)>0){
            warning("Following column_names are missing from the ",
                    table," table: \n",
                    paste(missingcolumns, collapse=", "))
        }
    }
}


#' Load raw data form the database
#'
#' Connect to the raw database and load all tradeflows
#' for the given product code,
#' for all years, in all directions, for all reporter and all partners.
#' @param productcode the code of on product
#' @export
loadrawdata <- function(productcode_, table_ = "raw_flow"){
    require(dplyr)
    if(is.null(getOption("tradeflowsDB"))){
        setdatabaseconfig()
    }
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    rawdata <- tbl(DBread, table_)
    dtf <- rawdata %>% filter(productcode == productcode_) %>%
        # Remove id as it is database specific and should not be carried through cleaning
        # Change this to remove all fields that are not part of column_names$efi
         select(-id, -lastchanged) %>%
        collect # forces computation and brings data back into a data.frame
    stopifnot(names(dtf) %in% column_names$efi)
    return(dtf)
}


#' Write data into the database
#'
#' Write data into the DB
#' @param dtf a data frame containing the data to be inserted
#' @export
writeenddata <- function(dtf, table="validated_flow"){
    if(is.null(getOption("tradeflowsDB"))){
        setdatabaseconfig()
    }
    require(RMySQL)
    db <- getOption("tradeflowsDB")
    DBwrite <- dbConnect(MySQL(), user=db["user"], host=db["host"],
                         password=db["password"], dbname=db["dbname"])
    result <- dbWriteTable(DBwrite, name=table,
                           value=data.frame(dtf), append=TRUE, row.names = FALSE)
    dbDisconnect(DBwrite)
    return(result)
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
    charcoal90 <- loadrawdata(440290)
    fibre$quantity2 <- 0
    writeenddata(fibre)
}

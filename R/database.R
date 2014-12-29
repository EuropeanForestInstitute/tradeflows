#' Sets database connection parameters as global R option tradeflowsDB
#'
#' Database connection parameters
#' Are storred in the installed version of the package
#' Run this command to find the configuration file:
#' system.file("config/databaseconf.R", package="tradeflows")
setdatabaseconfig <- function(){
    if(is.null(getOption("tradeflowsDB"))){
        message(paste("Loading database configuration from ",
                      system.file("config/databaseconf.R",
                                  package="tradeflows")))
        source(system.file("config/databaseconf.R", package="tradeflows"))
    }
}


#' Load raw data form the database
#'
#' Connect to the raw database and load all tradeflows
#' for the given product code,
#' for all years, in all directions, for all reporter and all partners.
#' @param productcode the code of on product
loadrawdata <- function(productcode_, table_ = "raw_flow"){
    require(dplyr)
    setdatabaseconfig()
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    rawdata <- tbl(DBread, sql(paste("SELECT * FROM",table_)))
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
writeenddata <- function(dtf, table="validated_flow"){
    require(RMySQL)
    DBwrite <- dbConnect(MySQL(), user="pauloef", host="hades.efi.int",
                      password="OEF4t4f", dbname="tradeflows")
    result <- dbWriteTable(DBwrite, name=table, value=dtf, append=TRUE, row.names = FALSE)
    dbDisconnect(DBwrite)
    return(result)
}


#' Number of rows in a database tables
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

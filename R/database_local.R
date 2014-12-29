#' Load raw data form the database
#'
#' Connect to the raw database and load all tradeflows
#' for the given product code,
#' for all years, in all directions, for all reporter and all partners.
#' @param productcode the code of on product
loadrawdata <- function(productcode_, table = "raw_trade_data"){
    require(dplyr)
    DBread <- src_mysql(user="tradeflows", host="localhost",
                    password="tradeflows", dbname="tradeflows")
    rawdata <- tbl(DBread, sql(paste("SELECT * FROM",table)))
    dtf <- rawdata %>% filter(productcode == productcode_) %>%
        collect # forces computation and brings data back into a data.frame
    return(dtf)
}


#' Write data into the database
#'
#' Write data into the DB
#' @param dtf a data frame containing the data to be inserted
writeenddata <- function(dtf, table="enddata"){
    require(RMySQL)
    DBwrite <- dbConnect(MySQL(), user="tradeflows", host="localhost",
                      password="tradeflows", dbname="tradeflows")
    result <- dbWriteTable(DBwrite, table, dtf, append=TRUE, row.names = FALSE)
    dbDisconnect(DBwrite)
    return(result)
}


writerawdata <- function(dtf){

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
    load("data-raw/comtrade/440799.RData")
    swd99 <- renamecolumns(dtf)
    nrowinDB("raw_trade_data")
    writeenddata(swd99, table="raw_trade_data")
    nrowinDB("raw_trade_data")
}

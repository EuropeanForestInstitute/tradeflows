# Functions that load raw data from the COMTRADE API and other sources

require(RJSONIO)
require(dplyr)

loadcomtrade_bycode <- function(productcode, reportercode, year){
    #'@description load a JSON file from comtrade in the temp directory
    #'              Converts the file to a dataframe
    #'@param reportercode : geographical area or country code
    #'@param productcode : code of the product
    #'@param year
    #'@return a dataframe
    jsonfile <- tempfile()
    url <- paste0("http://comtrade.un.org/api/get?px=HS&ps=",
                    year,"&r=",reportercode,
                    "&p=all&rg=all&cc=", productcode)
    download.file(url, destfile=jsonfile)
    json <- fromJSON(paste(readLines(jsonfile), collapse=""), nullValue = NA)
    unlink(jsonfile)
    stopifnot(json$validation$status$name == "Ok") # Check status
    dtf <- as.data.frame(do.call("rbind", json$dataset))
    return(dtf)
}


loadcomtrade <- function(reporter, product, year){
    #'@description convert names to the corresponding codes
    #' use the function wich loads by codes
    # product names beeing very long it might be usefull to allow
    # using part of the name if there is no ambiguity.
}


getcountrycode <- function(country){
    return(countrycode)
}


load_all_countries_for_a_product <- function(product, year){
    #' @description
    #' @param product name
}


convertcolumns <- function(dtf){
    # Creates column names which are more readable
    # Rename columns one by one based on the matching table
}


if (FALSE){
    # Show how functions above work on sample data
    loadbycomtradecode()
    load_by_name("germany","wood sawn or chip...",2011)
}

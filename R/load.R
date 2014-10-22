#' load raw data from the COMTRADE API and other sources
#'
#'@description load a JSON file from the comtrade API in the temp directory
#' The API is documented at http://comtrade.un.org/data/doc/api/
#' Converts the file to a dataframe
#'@import dplyr
#'@param reportercode : geographical area or country code
#'@param productcode : code of the product
#'@param year
#'@param px Trade data classification scheme
#'@param max maximum records returned
#'@return a dataframe
#'@export
loadcomtrade_bycode <- function(productcode, reportercode, year, px="HS", max=50000){
    jsonfile <- tempfile(fileext = ".json")
    url <- paste0("http://comtrade.un.org/api/get",
                  "?cc=", productcode,
                  "&r=", reportercode, "&p=all&rg=all", # All partners and flows
                  "&ps=", year,
                  "&px=", px,
                  "&max=", max, "&fmt=json")
    download.file(url, destfile=jsonfile)
    json <- RJSONIO::fromJSON(jsonfile, nullValue = NA)
    if (json$validation$status$name != "Ok"){ # Check status
        stop(json$validation)
    }
    dtf <- as.data.frame(do.call("rbind", json$dataset))
    unlink(jsonfile)
    return(dtf)
}


loadcomtrade <- function(product, reporter, year){
    #'@description convert names to the corresponding codes
    #' use the function wich loads by codes
    # product names beeing very long it might be usefull to allow
    # using part of the name if there is no ambiguity.
    loadcomtrade_bycode(product, reporter, year)
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

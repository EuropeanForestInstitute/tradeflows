#' load raw data from the COMTRADE API
#'
#'@description load a JSON file from the comtrade API in the temp directory
#' The API is documented at http://comtrade.un.org/data/doc/api/
#' Converts the file to a dataframe
#'@param reportercode  geographical area or country code
#'@param productcode  code of the product
#'@param year a string of year separated by commas
#'@param px Trade data classification scheme
#'@param max maximum records returned
#'@return a dataframe
#'@export
loadcomtrade_bycode <- function(productcode, reportercode, year,
                                px="HS", max=50000){
    jsonfile <- tempfile(fileext = ".json")
    url <- paste0("http://comtrade.un.org/api/get",
                  "?cc=", paste0(productcode, collapse = ","),
                  "&r=", reportercode,
                  "&p=all&rg=all", # All partners and flows
                  "&ps=", paste0(year, collapse = ","),
                  "&px=", px,
                  "&max=", max,
                  "&fmt=json")
    download.file(url, destfile=jsonfile)
    json <- jsonlite::fromJSON(jsonfile)
    if (json$validation$status$name != "Ok"){
        # If there was an error,
        # write it to a log file and give an error message
        stop(json$validation)
    }
    unlink(jsonfile)
    return(json$dataset)
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

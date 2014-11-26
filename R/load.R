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


#' load comtrade data for all countries, for a vector of product codes
#' this could be moved later.
#' @param productcode a vector of product codes
#' @export
loadcomtrade_all_countries <- function(productcode){
    for (code in tradeflows::reportercomtrade$reportercode){
        print(tradeflows::reportercomtrade[reportercomtrade$reportercode == code,])
        try(swd <- loadcomtrade_bycode(productcode, code, seq(2008,2012)))
        try(save(swd, file=paste0(reportercomtrade$text[reportercomtrade$id == id],
                                  ".RData")))
        try(write.csv(swd,file=paste0(reportercomtrade$text[reportercomtrade$id == id],
                                      ".csv")))
    }
}


# See example ans tests under docs/development
# http://appsso.eurostat.ec.europa.eu/nui/show.do?query=BOOKMARK_DS-016890_QID_-26F2D7BC_UID_-3F171EB0&layout=PERIOD,L,X,0;REPORTER,L,Y,0;PARTNER,L,Z,0;PRODUCT,B,Z,1;FLOW,L,Z,2;INDICATORS,C,Z,3;&zSelection=DS-016890PARTNER,EU28_EXTRA;DS-016890FLOW,2;DS-016890PRODUCT,44071031;DS-016890INDICATORS,VALUE_IN_EUROS;&rankName1=PARTNER_1_2_-1_2&rankName2=PRODUCT_1_2_-1_2&rankName3=FLOW_1_2_-1_2&rankName4=INDICATORS_1_2_-1_2&rankName5=PERIOD_1_0_0_0&rankName6=REPORTER_1_2_0_1&sortC=ASC_-1_FIRST&rStp=&cStp=&rDCh=&cDCh=&rDM=true&cDM=true&footnes=false&empty=false&wai=false&time_mode=NONE&time_most_recent=false&lang=EN&cfo=%23%23%23%2C%23%23%23.%23%23%23
loadeurostat <- function(){

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

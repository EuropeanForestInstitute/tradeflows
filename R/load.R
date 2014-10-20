require(RJSONIO)
require(dplyr)

load_comtrade <- function(reporting_code, product_code, year){
    #'@description load a JSON file from comtrade in the temp directory
    #              Converts the file to a dataframe
    #'@return a dataframe
    jsonfile <- tempfile()
    url <- paste0("http://comtrade.un.org/api/get?px=HS&ps=",
                    year,"&r=",reporting_code,
                    "&p=all&rg=all&cc=", product_code)
    download.file(url, destfile=jsonfile)
    json <- fromJSON(paste(readLines(jsonfile), collapse=""), nullValue = NA)
    unlink(jsonfile)
    dtf <- ldply(json$dataset, function(l) data.frame(l))
    return(dtf)
}


load_by_name <- function(reporting_country, product, year){
    #'@description convert names to the corresponding codes
    # product names beeing very long it might be usefull to allow
    # using part of the name if there is no ambiguity.
}



load_all_countries_for_a_product <- function(product, year){
    #' @description
    #' @param product name

}


convert_to_FAOSTAT_column_names <- function(dtf){
    # convert column names to FAOSTAT column names which are more readable
}

if (FALSE){
    # Show how functions above work on sample data
    load_comtrade()
    load_by_name("germany","wood sawn or chip...",2011)
}

#' load raw data from the COMTRADE API
#'
#'@description load a JSON file from the comtrade API in the temp directory
#' The API is documented at \url{http://comtrade.un.org/data/doc/api/}
#' Converts the file to a dataframe
#' Further information for developers is available at
#' \url{https://comtrade.un.org/Data/Doc/API}
#'@param productcode  code of the product
#'@param reportercode  geographical area or country code
#'@param year a vector of years (maximum 5), or the chain of character "recent"
#'@param freq the frequency: "A" Annual, "M" Monthly
#'@param px Trade data classification scheme
#'@param max maximum records returned
#'@return a dataframe
#'@examples
#' \dontrun{
#'library(tradeflows)
#' # Load trade data from Comtrade
#' # "other sawnwood" trade France in 2018
#'loadcomtradebycode(440799, 251, 2018)
#'}
#'@export
loadcomtradebycode <- function(productcode, reportercode,
                               year, freq = "A",
                                px = "HS", max = 50000,
                                logfile = FALSE){
    jsonfile <- tempfile(fileext = ".json")
    url <- paste0("http://comtrade.un.org/api/get",
                  "?cc=", paste0(productcode, collapse = ","),
                  "&r=", paste0(reportercode, collapse = ","),
                  "&p=all&rg=all", # All partners and flows
                  "&ps=", paste0(year, collapse = ","),
                  "&freq=",freq,
                  "&px=", px,
                  "&max=", max,
                  "&fmt=json")
    download.file(url, destfile=jsonfile)
    json <- jsonlite::fromJSON(jsonfile)

    if (json$validation$status$name != "Ok"){
        # If there was an error,
        # write it to a log file and give an error message
        stop(json$validation)
        # write geterrmessage() to a log file
    }
    unlink(jsonfile)
    return(json$dataset)
}


#' Load flows for all countries, in all directions available in comtrade
#' for a given product.
#'
#' Data is saved in a RDATA file and optionally in a csv file.
#' @param productcode a vector of product codes
#' @param year a vector of years (maximum 5), or the chain of character "recent"
#' @param path directory to write RDATA and csv files
#' @param writecsv logical whether to store data in a csv file
#' @param ... further parameters passed to \code{\link{loadcomtradebycode}},
#' @export
loadcomtradeallreporters <- function(productcode, year = "recent",
                                     path = "", writecsv = FALSE, ...){
    dtf <- data.frame()
    # Create groups of 3 reporter, to overcome API limitation
    # of 100 downloads per hour
    # more details in docs/development/comtrade.Rmd
    reportercomtrade$group <- round(as.numeric(row.names(reportercomtrade)) /3)
    reporter_list <- split(reportercomtrade$reportercode,
                           as.factor(reportercomtrade$group))
    # Remove "all", because it won't be accepted for reasons of query complexity
    reporter_list[reporter_list=="all"] <- NULL
    # Loop on group of reporters
    for (g in reporter_list){
        print(g)
        try(dtf <- rbind(dtf,
                         loadcomtradebycode(productcode, g, year,
                                             logfile=TRUE, ...)))
    }
    save(dtf, file = file.path(path, paste0(productcode,".RData")))
    if(writecsv){
        try(write.csv(dtf,file = file.path(path, paste0(productcode,".csv"))))
    }
}


#' Load flows for all countries, with a 1 hour pause between products
#'
#' Load all world trade flows for the vector of products.
#' @param products a vector of product codes
#' @param year a vector of years (maximum 5), or the chain of character "recent"
#' @param path directory to write RDATA and csv files
#' @param writecsv logical whether to store data in a csv file
#' @param pause length of the pause in seconds
#' @param ... further parameters passed to \code{\link{loadcomtradeallreporters}},
#' then to \code{\link{loadcomtradebycode}}
#' @export
loadcomtradewithpause <- function(products, year="recent", path="",
                                  writecsv=FALSE, pause=3601, ...){
    # loop on the vector productcode,
    # pause 1 hour between each product to stay within the comtrade API limit
    for (productcode in products){
        loadcomtradeallreporters(productcode, year, path, writecsv, ...)
        message("Waiting for ",pause," seconds, until Comtrade API download is allowed again.")
        Sys.sleep(pause)
    }
}


# # On the server I have put datasets together in this way
# swdall <- data.frame()
# for (f in list.files(".","RData")){
#     load(f);swdall <- rbind(swd,swdall)
# }
# save(swdall, file="sawnwood_all.RData")
# swdall2 <- tradeflows::renamecolumns(swdall)
# create_completeness_report(swdall2)

# See example and tests under docs/development
# http://appsso.eurostat.ec.europa.eu/nui/show.do?query=BOOKMARK_DS-016890_QID_-26F2D7BC_UID_-3F171EB0&layout=PERIOD,L,X,0;REPORTER,L,Y,0;PARTNER,L,Z,0;PRODUCT,B,Z,1;FLOW,L,Z,2;INDICATORS,C,Z,3;&zSelection=DS-016890PARTNER,EU28_EXTRA;DS-016890FLOW,2;DS-016890PRODUCT,44071031;DS-016890INDICATORS,VALUE_IN_EUROS;&rankName1=PARTNER_1_2_-1_2&rankName2=PRODUCT_1_2_-1_2&rankName3=FLOW_1_2_-1_2&rankName4=INDICATORS_1_2_-1_2&rankName5=PERIOD_1_0_0_0&rankName6=REPORTER_1_2_0_1&sortC=ASC_-1_FIRST&rStp=&cStp=&rDCh=&cDCh=&rDM=true&cDM=true&footnes=false&empty=false&wai=false&time_mode=NONE&time_most_recent=false&lang=EN&cfo=%23%23%23%2C%23%23%23.%23%23%23
loadeurostat <- function(){

}

getcountrycode <- function(country){
    return(countrycode)
}


# load_all_countries_for_a_product <- function(product, year){
#     #' @description
#     #' @param product name
# }




if (FALSE){
    # Show how functions above work on sample data
    loadcomtradebycode
}

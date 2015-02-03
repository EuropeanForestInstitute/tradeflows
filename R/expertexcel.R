#' Clean and export to Excel for the expert users
#'
#' Keep price, conversion factor andflow choice in a separate table
#' for further analysis
#' @param dtf data frame
#' @param filename path and name of the output files,
#' @param ... furhter arguments passed to clean()
#'  without extensions
clean2excel <- function(dtf, filenamestart, ...){
    require(xlsx)
    require(reshape2)
    results <- clean(dtf,outputalltables = TRUE, ...)
    # Product description with number of reporter countries
    results$productclassification <- results$dtf %>%
        select(productcode, classification, year, productdescription, reporter) %>%
        unique %>%
        group_by(productcode, classification, year, productdescription) %>%
        summarise(n = n()) %>% arrange(classification) %>% data.frame %>%
        mutate(year = paste0("Y",year)) %>% # replace dcast default X by a Y
        dcast(productcode + classification + productdescription ~ year,
              value.var = "n")


    results$dtf <- results$dtf %>%
        # remove product description to get a smaller csv file
        select(-productdescription) %>%
        # arrange by year, reporter and flow
        data.frame %>%
        arrange(year, reporter, flow)

    # Extract information from flags
    results$flags <- extractflags(results$dtf)

    # Extract global information
    results$worldchange <- results$dtf %>%
        group_by(year) %>%
        summarise(quantityraw = sum(quantityraw, na.rm=TRUE),
                  quantity = sum(quantity, na.rm=TRUE),
                  quantitychange = quantity - quantityraw,
                  changeratio = quantitychange / quantityraw,
                  nrow = n()) %>%
        mutate(changeratio = quantitychange / quantityraw)


    # Write trade flows data frame to  csv
    # (to be compressed later)
    write.csv(results$dtf,
              paste0(filenamestart, ".csv"), row.names = FALSE)

    # Write other data frames to Excel
    # Convert all data frame to data frames
    # some are grouped data frames and this causers issues
    # when writing to Excel with row.names = FALSE
    results <- lapply(results, data.frame)

    wb <- createWorkbook()
    #     sheet1  <- createSheet(wb, sheetName="Trade flows")
    #     addDataFrame(results$dtf, sheet1, row.names=TRUE)
    sheet2  <- createSheet(wb, sheetName="Conversion factors")
    # Issue with row.names = FALSE : columns are not written correctly
    addDataFrame(results$conversionfactor, sheet2, row.names=FALSE)
    sheet3  <- createSheet(wb, sheetName="Unit prices")
    addDataFrame(results$price, sheet3, row.names=FALSE)
    sheet4  <- createSheet(wb, sheetName="Choice description")
    addDataFrame(results$choice, sheet4, row.names=FALSE)
    sheet5  <- createSheet(wb, sheetName="Product classification")
    addDataFrame(results$productclassification, sheet5, row.names=FALSE)
    addDataFrame(results$flags, row.names=FALSE,
                 sheet = createSheet(wb, sheetName="Quantity change by flag"))
    sheet7  <- createSheet(wb, sheetName="Quantity change world")
    addDataFrame(results$worldchange, sheet7, row.names=FALSE)
    saveWorkbook(wb, paste0(filenamestart, "_cfupchoice.xlsx"))

    # Compress csv and excel file in a zip archive
    zip(paste0(filenamestart, ".zip"),
        files = c(paste0(filenamestart, ".csv"),
                     paste0(filenamestart, "_cfupchoice.xlsx")))
}

#' Extract information from flag estimates
#'
#' @param dtf a cleaned data frame containing trade flows
#' @export
extractflags <- function(dtf){
    # Change all NA values to calculated the difference
    # in quantities
    dtf[is.na(dtf)] <- 0
    flags <- dtf %>%
        filter(flow %in% c("Import", "Export")) %>%
        # Remove EU28 reporter
        filter(!reporter %in% c("EU-28")) %>%
        # Remove EU-28 and World partner
        filter(!partner %in%c("EU-28", "World")) %>%
        arrange(year) %>%
        mutate(quantitychange = quantity - quantityraw) %>%
        group_by(flag, year) %>%
        summarise(quantityraw = sum(quantityraw),
                  quantity = sum(quantity),
                  quantitychange = sum(quantitychange),
                  changeratio = quantitychange / quantityraw,
                  nrow = n()) %>%
        melt(id=c("flag", "year")) %>%
        data.frame %>%
        arrange(variable, flag) %>%
        dcast(variable + flag ~ year, value.var = "value")
    return(flags)
}


#' Clean from a database table to Excel, for expert analysis.
#'
#' @param productcode code of a product
#' @param tableread names of the database table to read
cleandb2excel <-function(productcode, tableread){

}


#' Clean from raw data file to Excel and a csv file
#' @param rawfile name of a .RData file containing raw trade flows
#' @param ... further arguements passed to clean()
#' @rdname clean2excel
#' @export
cleanrdata2excel <- function(rawfilename, ...){
    load(rawfilename)
    filenamestart <- gsub("comtrade", "excel", rawfilename)
    filenamestart <- gsub(".RData", "", filenamestart)
    dtf %>% renamecolumns %>%
        clean2excel(filenamestart = filenamestart, ...)
}


if (FALSE){
    # Write other sawnwood to Excel for expert analysis
    # 440799
    # All changes, including replacing quantity by partner value
    # and "shaving" prices
    cleanrdata2excel("data-raw/comtrade/440799.RData")

    # Only quantity estimates wolrd aggregated conversion factors
    cleanrdata2excel("data-raw/comtrade/440799.RData",
                     geoaggregation = "world",
                     replacebypartnerquantity = FALSE,
                     shaveprice = FALSE)
    # Only quantity estimates, regional aggregated conversion factors
    cleanrdata2excel("data-raw/comtrade/440799.RData",
                     geoaggregation = "region",
                     replacebypartnerquantity = FALSE,
                     shaveprice = FALSE)
    # 440799 estimated quantity based on the weight and unit prices + replacing by partner flow
    cleanrdata2excel("data-raw/comtrade/440799.RData",
                     replacebypartnerquantity = TRUE,
                     shaveprice = FALSE)
    # 440799 estimated quantity based on the weight and unit prices + shaving prices
    cleanrdata2excel("data-raw/comtrade/440799.RData",
                     replacebypartnerquantity = FALSE,
                     shaveprice = TRUE)

    # 440721 - Lumber, Meranti red, Meranti Bakau, White Lauan etc
    cleanrdata2excel("data-raw/comtrade/440721.RData")
    # 440729 - Lumber, tropical wood ne
    cleanrdata2excel("data-raw/comtrade/440729.RData")
    # 940161- Wooden seates
    cleanrdata2excel("data-raw/comtrade/940161.RData")
}

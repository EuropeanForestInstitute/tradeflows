#' Clean and export to Excel for the expert users
#'
#' Keep price, conversion factor andflow choice in a separate table
#' for further analysis
#' @param dtf data frame
#' @param filename path and name of the output files,
#'  without extensions
clean2excel <- function(dtf, filenamestart){
    require(xlsx)
    results <- clean(dtf,outputalltables = TRUE)
    # Product description with number of reporter countries
    results$productclassification <- results$dtf %>%
        select(productcode, classification, year, productdescription, reporter) %>%
        unique %>%
        group_by(productcode, classification, year, productdescription) %>%
        summarise(n = n()) %>% arrange(classification) %>% data.frame %>%
        dcast(productcode + classification + productdescription ~ year,
              value.var = "n")


    results$dtf <- results$dtf %>%
        # remove product description to get a smaller csv file
        select(-productdescription) %>%
        # arrange by year, reporter and flow
        data.frame %>%
        arrange(year, reporter, flow)


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
    addDataFrame(results$conversionfactorworld, sheet2, row.names=FALSE)
    sheet3  <- createSheet(wb, sheetName="Regional unit prices")
    addDataFrame(results$priceregion, sheet3, row.names=FALSE)
    sheet4  <- createSheet(wb, sheetName="Choice description")
    addDataFrame(results$choice, sheet4, row.names=FALSE)
    sheet5  <- createSheet(wb, sheetName="Product classification")
    addDataFrame(results$productclassification, sheet5, row.names=FALSE)
    saveWorkbook(wb, paste0(filenamestart, "_cfupchoice.xlsx"))

    # Compress csv and excel file in a zip archive
    zip(paste0(filenamestart, ".zip"),
        files = c(paste0(filenamestart, ".csv"),
                     paste0(filenamestart, "_cfupchoice.xlsx")))
}


cleandb2excel <-function(product){

}


cleanrdata2excel <- function(rawfile){

}


if (FALSE){
    # Write other sawnwood to Excel for expert analysis
    load("data-raw/comtrade/440799.RData")
    dtf %>% renamecolumns %>%
        clean2excel(filenamestart = "data-raw/excel/440799")
    # 440721 - Lumber, Meranti red, Meranti Bakau, White Lauan etc
    load("data-raw/comtrade/440721.RData")
    dtf %>% renamecolumns %>%
        clean2excel(filenamestart = "data-raw/excel/440721")
    # 440729 - Lumber, tropical wood ne
    load("data-raw/comtrade/440729.RData")
    dtf %>% renamecolumns %>%
        clean2excel(filenamestart = "data-raw/excel/440729")
}

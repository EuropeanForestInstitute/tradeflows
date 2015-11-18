#' Clean and export price and conversion factor to Excel
#'
#' Save price, conversion factor and flow choice in a separate table
#' for further analysis.
#' Use the argument file to specify where the Excel file will be located.
#' @param dtf data frame
#' @param file a character string naming an Excel file, should end by ".xlsx"
#' @param path a character string specifying where the excel file will be saved
#' @param returnresults TRUE if the list of results should be returned
#' usefull for the function cleanrdata2excel.
#' @param ... further arguments passed to \code{\link{clean()}}
#'  without extensions
#' @export
clean2excel <- function(dtf, file, path = "data-raw/excel", returnresults = FALSE, ...){
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

    message("Add a description of each sheet, taken from a csv file.")

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


    # Write other data frames to Excel
    # Convert all data frame to data frames
    # some are grouped data frames and this causers issues
    # when writing to Excel with row.names = FALSE
    results <- lapply(results, data.frame)


    # Add data frame with description
    addDataFrame2 <- function(wb, dtf, sheetname){
        sheet  <- createSheet(wb, sheetName = sheetname)
        addDataFrame(dtf, sheet, row.names=FALSE)

        # Add description if available
        sheetdescription <- column_description %>%
            filter(sheet == sheetname) %>%
            select(-sheet)
        if (nrow(sheetdescription)>0){
            addDataFrame(sheetdescription, sheet, row.names=FALSE,
                         startColumn = ncol(results$price) + 2)
        }
    }

    wb <- createWorkbook()
    #     sheet1  <- createSheet(wb, sheetName="Trade flows")
    #     addDataFrame(results$dtf, sheet1, row.names=TRUE)
    sheet2  <- createSheet(wb, sheetName="Conversion factors")
    # Issue with row.names = FALSE : columns are not written correctly
    addDataFrame(results$conversionfactor, sheet2, row.names=FALSE)
    addDataFrame2(wb, results$price, "Unit prices")
#     sheet3  <- createSheet(wb, sheetName="Unit prices")
#     addDataFrame(results$price, sheet3, row.names=FALSE)
#     addDataFrame(sheetdescription("Unit prices"), sheet3, row.names=FALSE,
#                  startColumn = ncol(results$price) + 1)
    sheet4  <- createSheet(wb, sheetName="Choice description")
    addDataFrame(results$choice, sheet4, row.names=FALSE)
    sheet5  <- createSheet(wb, sheetName="Product classification")
    addDataFrame(results$productclassification, sheet5, row.names=FALSE)
    addDataFrame(results$flags, row.names=FALSE,
                 sheet = createSheet(wb, sheetName="Quantity change by flag"))
    sheet7  <- createSheet(wb, sheetName="Quantity change world")
    addDataFrame(results$worldchange, sheet7, row.names=FALSE)
    saveWorkbook(wb, file.path(path,file))

    if(returnresults){ # You might want to return invisibly instead?
        return(results)
    }
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


#' \code{cleandb2excel} cleans from a database table to Excel.
#' @param productcode code of a product
#' @param tableread names of the database table to read
#' @rdname clean2excel
#' @export
cleandb2excel <-function(productcode, file = paste0(productcode, ".xlsx"),
                         tableread = "raw_flow_yearly" , ...){
    dtf <- readdbproduct(productcode, tableread)
    if(identical(nrow(dtf), 0L)){
        stop("No data for product ", productcode, " in ", tableread)
    }
    clean2excel(dtf, returnresults = FALSE, file = file,  ...)
}


#' \code{cleanrdata2excel} cleans from a raw data file to Excel and
#' write the cleaned dataset to a csv file.
#' @param rawfile name of a .RData file containing raw trade flows
#' @rdname clean2excel
#' @export
cleanrdata2excel <- function(rawfilename, ...){
    load(rawfilename)
    file <- gsub("comtrade", "excel", rawfilename)
    file <- gsub(".RData", ".xlsx", file)
    results <- dtf %>% renamecolumns %>%
        clean2excel(file = file,
                    returnresults = TRUE, ...)

    # Write trade flows data frame to  csv
    # (to be compressed later)
    write.csv(results$dtf,
              paste0(file, ".csv"), row.names = FALSE)

    # Compress csv and excel file in a zip archive
    zip(paste0(file, ".zip"),
        files = c(paste0(file, ".csv"),
                     paste0(file, ".xlsx")))
}


if (FALSE){ #### Clean from raw RDATA files ####
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


if(FALSE){ ### Clean from the database ####
    library(tradeflows)
    cleandb2excel(440799)
    # Would be nice to automate this file prefix with a function taking it from the JFSQ-2 code
    cleandb2excel(441114, "mdf441114.xlsx")
    cleandb2excel(480459, "carton480459.xlsx")
    cleandb2excel(440110, "woodfuel440110.xlsx")
    cleandb2excel(4707, "recoveredpaper4707.xlsx")
    # TO different path
    cleandb2excel(480459, path="/tmp")
}

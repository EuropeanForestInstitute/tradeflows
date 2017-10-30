#' Extract tonnage prices
#'
#' Takes a data frame of tradeflows as input.
#' @rdname extractprices
#' @export
extractpricew <- function(dtf, lowercoef= 1, uppercoef=1,
                          grouping = c("flow", "regionreporter", "year", "unit")){
    # Grouping variables should be present in the data frame
    stopifnot(grouping %in% names(dtf))
    # pricew should be present in the data frame
    stopifnot("pricew" %in% names(dtf))
    # replace infinite price values by NA for the mean price
    # calculation
    dtf$pricew[is.infinite(dtf$pricew)] <- NA

    dtf %>%
        # Price should not be NA and not be infinite
        filter(!is.na(pricew) & !is.infinite(pricew)) %>%
        # Calculate prices by grouping variables
        group_by_(.dots = grouping) %>%
        summarise(lowerpricew = lowercoef * quantile(pricew, 0.05, names=FALSE),
                  medianpricew = median(pricew),
                  upperpricew = uppercoef * quantile(pricew, 0.95, names=FALSE),
                  averagepricew = mean(pricew),
                  weightedaveragepricew = sum(tradevalue)/ sum(weight))
}


#' @rdname shaveprice
#' @export
shavepricew <- function(dtf, verbose = getOption("tradeflows.verbose",TRUE)){
    # Split flows which have pricews out of bounds from those which don't
    dtf <- dtf %>% mutate(rawpricew = pricew,
                          pricew = tradevalue / weight)
    # Deal with missing pricews and missing pricew bounds
    dtfnoboundpricew <- dtf %>%
        filter((is.na(lowerpricew)|is.na(upperpricew)) & !is.na(pricew))
    dtfnopricew <- dtf %>%
        filter(is.na(pricew))
    message(nrow(dtfnoboundpricew), " rows where pricew bounds were not available.")
    message(nrow(dtfnopricew), " rows where pricew were not available.")
    dtfinbound <- dtf %>%
        filter((lowerpricew <= pricew & pricew <= upperpricew))
    # This is where the modification takes place
    # If pricew < lowerpricew, then weight = tradevalue / lowerpricew
    # If pricew > upperpricew, then weight = tradevalue / upperpricew
    dtfoutbound <- dtf %>%
        # Will contain also Inf pricews
        filter(pricew < lowerpricew | upperpricew < pricew) %>%
        mutate(weight = ifelse(pricew < lowerpricew,
                               tradevalue / lowerpricew,
                               tradevalue / upperpricew),
               flag = flag + 400)
    dtfresult <- rbind(dtfinbound, dtfoutbound, dtfnoboundpricew, dtfnopricew)
    stopifnot(nrow(dtf) == nrow(dtfresult))
    if (verbose){
        message(nrow(dtfoutbound), " rows had a pricew too high or too low")
        message("Readjusting weight so that pricew is within the lower and upper bounds",
                changeflowmessage(dtf, dtfresult, variable = "weight"))
    }
    return(dtfresult)
}


#' @rdname shaveprice
#' @export
shaveconversion <- function(dtf, verbose = getOption("tradeflows.verbose",TRUE)){
    # Split flows which have conversion factors out of bounds from those which don't
    dtf <- dtf %>% mutate(rawconversion = conversion,
                          conversion = weight / quantity)
    # Deal with missing conversion factors and missing conversion factor bounds
    dtfnoboundconversion <- dtf %>%
        filter((is.na(lowerconversion)|is.na(upperconversion)) & !is.na(conversion))
    dtfnoconversion <- dtf %>%
        filter(is.na(conversion))
    message(nrow(dtfnoboundconversion), " rows where conversion bounds were not available.")
    message(nrow(dtfnoconversion), " rows where conversion were not available.")
    dtfinbound <- dtf %>%
        filter((lowerconversion <= conversion & conversion <= upperconversion))
    # This is where the modification takes place
    # If conversion < lowerconversion, then weight = tradevalue / lowerconversion
    # If conversion > upperconversion, then weight = tradevalue / upperconversion
    dtfoutbound <- dtf %>%
        # Will contain also Inf conversion factors
        filter(conversion < lowerconversion | upperconversion < conversion) %>%
        mutate(quantity = ifelse(conversion < lowerconversion,
                               weight / lowerconversion,
                               weight / upperconversion),
               flag = flag + 400)
    dtfresult <- rbind(dtfinbound, dtfoutbound, dtfnoboundconversion, dtfnoconversion)
    stopifnot(nrow(dtf) == nrow(dtfresult))
    if (verbose){
        message(nrow(dtfoutbound), " rows had a conversion too high or too low")
        message("Readjusting quantity so that conversion is within the lower and upper bounds",
                changeflowmessage(dtf, dtfresult, variable = "weight"))
    }
    return(dtfresult)
}


#' Clean Comext Monthly data
#'
#' @param RMySQLcon database connection object created by RMySQL \code{\link[DBI]{dbConnect}}
#' @param tablearchive character name of a monthly archive table
#' @param tablerecent character name of a monthly recent table
#' @param tableprice character name of a price table
#' @param tablecv character name of a conversion factor table
#' @return \code{cleancomextmonthly1product} invisibly returns a data frame
#' with all columns generated during the cleaning process.
#' @examples \dontrun{ # Clean product and country codes
#' # Connect to the database
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
#' # Clean product 44079910
#' cleancomextmonthly1product(con ,
#'                            productanalysed = "44071091",
#'                            tablearchive = "raw_comext_monthly_2016S1",
#'                            tablerecent = "raw_comext_monthly_201709",
#'                            tablewrite = "vld_comext_monthly",
#'                            tableprice = "vld_comext_price",
#'                            tablepricew = "vld_comext_pricew",
#'                            tablecv = "vld_comext_cv")
#' cleancomextmonthly(con ,
#'                    tablearchive = "raw_comext_monthly_2016S1",
#'                    tablerecent = "raw_comext_monthly_201709",
#'                    tablewrite = "vld_comext_monthly",
#'                    tableprice = "vld_comext_price",
#'                    tablepricew = "vld_comext_pricew",
#'                    tablecv = "vld_comext_cv")
#' # Disconnect from the database
#' RMySQL::dbDisconnect(con)
#' }
#' @export
cleancomextmonthly1product <- function(RMySQLcon,
                                       productanalysed,
                                       tablearchive,
                                       tablerecent,
                                       tablewrite,
                                       tableprice = "vld_comext_price",
                                       tablepricew = "vld_comext_pricew",
                                       tablecv = "vld_comext_cv"){
    message("\n\nCleaning product code: ", productanalysed)
    message("Repetitive elements of this long function could be",
            " placed in several smaller functions.")
    message("starting with data loading until the comment # Are there any missing years?")

    # load trade flows from the database into a data frame
    message("Load recent data from ", tablerecent, "...")
    dtfr <- tbl(RMySQLcon, tablerecent) %>%
        filter(productcode == productanalysed) %>%
        # Add quantity units
        eutradeflows::addunit2tbl(RMySQLcon,
                                  maintbl = .,
                                  tableunit = "vld_comext_unit")  %>%
        collect()
    beginrecentdata <- min(dtfr$period)

    # Load archive data, for periods before the begin of recent data
    message("Load archive data from ",tablearchive, "...")
    dtfa <- tbl(RMySQLcon, tablearchive) %>%
        filter(productcode == productanalysed &
                   period < beginrecentdata) %>%
        # Add quantity units
        eutradeflows::addunit2tbl(RMySQLcon,
                                  maintbl = .,
                                  tableunit = "vld_comext_unit")  %>%
        collect()    # load trade flows from the database into a data frame

    # Combine archive and recent data
    dtf <- rbind(dtfa, dtfr)

    # Extract year with integer division
    dtf$year <- dtf$period %/% 100
    years <- unique(dtf$year)
    # Are there any missing years?
    if(!identical(min(years):max(years), as.integer(years))){
        warning("These years are missing from the data: ",
                setdiff(min(years):max(years), years))
    }



    # Remove unnecessary objects
    rm(dtfa)
    rm(dtfr)

    # Add prices and conversion factors to the data frame
    dtf <- addconversionfactorandprice(dtf)
    # Prepare conversion factors and prices,
    # compute median prices, price bounds and conversion factors in a data frame
    # Edit column names to match comtrade columns
    dtf <- mutate(dtf,
                  # `unit` column hardcoded in estimatequantity()
                  unit = unitcode,
                  # Add the flag as it will be needed
                  flag = 0)

    # Extract prices and conversion factors
    price <- extractprices(dtf, grouping = c("productcode", "flowcode",
                                             "year", "unit"))
    pricew <- extractpricew(dtf, grouping = c("productcode", "flowcode",
                                              "year", "unit"))
    # If price and pricew have the same number of lines, it should be
    # possible to join them
    price <- price %>%
        left_join(pricew, by = c("productcode", "flowcode", "year", "unit"))

    cvf <- extractconversionfactors(dtf, grouping = c("productcode", "flowcode",
                                                      "year", "unit"))
    # Store rows before the change
    nrowbeforechange <- nrow(dtf)

    # Estimate quantity
    message("\n\n # Move the price and cvf adding part out of estimatequantity")
    message("It might be more productive")
    dtf <- estimatequantity(dtf, price, cvf)

    # Shave price
    # based on upper and lower prices added above
    # by the estimatequantity() function
    dtf <- shaveprice(dtf)

    # Place here shavepricew()
    #       then shaveconversion()
    # That's it

    # count(dtf, flag)

    # Before writing prices back to the database, rename some columns

    dtf <- mutate(dtf,
                  unitcode = unit)

    # Use database columns to select which columns to keep in the
    # data frame
    # get column names
    columnswrite  <- RMySQL::dbListFields(RMySQLcon, "vld_comext_monthly_template")
    db_dtf <- select_(dtf, .dots = columnswrite)
    # Delete existing data for the given product
    query <- paste("DELETE FROM ", tablewrite,
                   "WHERE productcode = ", productanalysed)
    res <- RMySQL::dbSendQuery(RMySQLcon, query)
    RMySQL::dbClearResult(res)
    message(paste("Writing", nrow(dtf), "flows to the database."))
    # Write dtf
    RMySQL::dbWriteTable(con, name = tablewrite,
                         value = db_dtf, append=TRUE, row.names = FALSE)

    # # Write prices and conversion factors to the database
    # message("add units to the price and cv table structures, no need in pricew")
    # Prices are encoded as years
                  # years encoded as 2018, will be different to the period 201852
                  # period = year,
    # RMySQL::dbWriteTable(con, name = tableprice,
    #                      value = price, append=TRUE, row.names = FALSE)
    # RMySQL::dbWriteTable(con, name = tablepricew,
    #                      value = pricew, append=TRUE, row.names = FALSE)
    # RMySQL::dbWriteTable(con, name = tablecv,
    #                      value = cvf, append=TRUE, row.names = FALSE)
    return(invisible(dtf))
}


#' @param ... parameters passed to cleancomextmonthly1product
#' @rdname cleancomextmonthly1product
cleancomextmonthly <- function(RMySQLcon,
                               productanalysed,
                               tablearchive,
                               tablerecent,
                               tablewrite,
                               ...){
    # Find all products in the recent and archive table
    dtfr <- tbl(RMySQLcon, tablerecent)

    # Clean all products in a loop

}


#' Extract tonnage prices
#'
#' Takes a data frame of tradeflows as input.
#' @rdname extractprices
#' @export
extractpricew <- function(dtf, lowercoef= 0.5, uppercoef=2,
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
        summarise(lowerpricew = lowercoef * quantile(pricew, 0.25, names=FALSE),
                  medianpricew = median(pricew),
                  upperpricew = uppercoef * quantile(pricew, 0.75, names=FALSE),
                  averagepricew = mean(pricew),
                  weightedaveragepricew = sum(tradevalue)/ sum(weight))
}


#' Clean Comext Monthly data
#'
#' @param RMySQLcon database connection object created by RMySQL \code{\link[DBI]{dbConnect}}
#' @param tablearchive character name of a monthly archive table
#' @param tablerecent character name of a monthly recent table
#' @param tableprice character name of a price table
#' @param tablecv character name of a conversion factor table
#' @examples \dontrun{ # Clean product and country codes
#' # Connect to the database
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
#' # Clean product 44079910
#'
#' # Disconnect from the database
#' RMySQL::dbDisconnect(con)
#' }
#' @export
cleancomextmonthly1product <- function(RMySQLcon,
                                       productanalysed,
                                       tablearchive,
                                       tablerecent,
                                       tableprice,
                                       tablepricew,
                                       tablecv){
    message("Repetitive elements of this long function could be",
            " placed in several smaller functions.")
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

    # Remove unnecessary objects
    rm(dtfa)
    rm(dtfr)

    # Add prices and conversion factors to the data frame
    dtf <- addconversionfactorandprice(dtf)
    # Prepare conversion factors and prices,
    # compute median prices, price bounds and conversion factors in a data frame
    # Extract year with integer division
    dtf$year <- dtf$period %/% 100
    years <- unique(dtf$year)
    # Are there any missing years?
    if(!identical(min(years):max(years), as.integer(years))){
        warning("These years are missing from the data: ",
                setdiff(min(years):max(years), years))
    }

    # Edit column names to matche comtrade columns
    dtf <- mutate(dtf,
                  # `unit` column hardcoded in estimatquantity()
                  unit = unitcode,
                  flag = 0)

    price <- extractprices(dtf, grouping = c("productcode", "flowcode",
                                             "year", "unit"))
    pricew <- extractpricew(dtf, grouping = c("productcode", "flowcode",
                                              "year", "unit"))
    cvf <- extractconversionfactors(dtf, grouping = c("productcode", "flowcode",
                                                      "year", "unit"))
    # Store rows before the change
    nrowbeforechange <- nrow(dtf)

    # Estimate quantity
    dtf <- estimatequantity(dtf, price, cvf)

    # Shave price
    # based on upper and lower prices added above
    # by the estimatequantity() function
    dtf <- shaveprice(dtf)


    count(dtf, flag)

    # Before writing prices back to the database, rename some columns

    dtf <- mutate(dtf,
                  unitcode = unit)

    # Use database columns to select which columns to keep in the
    # data frame
    # get column names
    columnswrite  <- RMySQL::dbListFields(RMySQLcon, "vld_comext_monthly_template")
    dtf <- select_(dtf, .dots = columnswrite)
    # Delete existing data for the given product
    query <- paste("DELETE FROM ", tablewrite,
                   "WHERE productcode = ", productanalysed)
    res <- RMySQL::dbSendQuery(RMySQLcon, query)
    RMySQL::dbClearResult(res)
    message(paste("Writing", nrow(dtf), "flows to the database."))
    # Write dtf
    RMySQL::dbWriteTable(con, name = tablewrite,
                         value = dtf, append=TRUE, row.names = FALSE)

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


}

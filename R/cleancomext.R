

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
                                       tablearchive,
                                       tablerecent,
                                       tableprice,
                                       tablecv){
    # load trade flows from the database into a data frame
    # Load archive data
    dtfa <- tbl(RMySQLcon, tablearchive) %>%
        filter(productcode == productanalysed) %>%
        # Add quantity units
        eutradeflows::addunit2tbl(RMySQLcon,
                                  maintbl = .,
                                  tableunit = "vld_comext_unit")  %>%
        collect()

    # Load recent data
    dtfr <- tbl(RMySQLcon, tablerecent) %>%
        filter(productcode == productanalysed) %>%
        # Add quantity units
        eutradeflows::addunit2tbl(RMySQLcon,
                                  maintbl = .,
                                  tableunit = "vld_comext_unit")  %>%
        collect()

    # Combine archive and recent data
    dtf <- rbind(dtfa, dtfr)

    # Remove unnecessary objects
    rm(dtfa)
    rm(dtfr)

    # Prepare conversion factors and prices,
    # compute median prices, price bounds and conversion factors in a data frame
    price <- tradeflows::extractprices(dtf, grouping = c("flow", "year", "unit"))
    conversionfactor <- tradeflows::extractconversionfactors(dtf)

    # calculate missing quantities
    # shave prices
    # store prices and conversion factors in the database
    # store modified trade flows into the database

}

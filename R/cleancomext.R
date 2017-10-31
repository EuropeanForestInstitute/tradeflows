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
               flag = flag + 100)
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
               flag = flag + 200)
    dtfresult <- rbind(dtfinbound, dtfoutbound, dtfnoboundconversion, dtfnoconversion)
    stopifnot(nrow(dtf) == nrow(dtfresult))
    if (verbose){
        message(nrow(dtfoutbound), " rows had a conversion too high or too low")
        message("Readjusting quantity so that conversion is within the lower and upper bounds",
                changeflowmessage(dtf, dtfresult, variable = "weight"))
    }
    return(dtfresult)
}


#' Load all archive and recent Comext data for one product
#'
#' The function will use the database connector provided as
#' RMySQLcon argument.
#' It will load the product from an archive table and a recent
#' table. The function will combine the archive table and
#' the recent table in one dataframe and will return a dataframe.
#' @return a  data frame containing Comext trade flows for the given product
#' @examples \dontrun{ # Clean product and country codes
#' # Connect to the database
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "tradeflows")
#' # Load data for product 44079910
#' dtf <- loadcomext1product(con,
#'                           productanalysed = "44071091",
#'                           tablearchive = "raw_comext_monthly_2016S1",
#'                           tablerecent = "raw_comext_monthly_201709")
#' head(dtf)
#' unique(dtf$year)
#' # Disconnect from the database
#' RMySQL::dbDisconnect(con)
#' }
#' @export
loadcomext1product <- function(RMySQLcon,
                               productanalysed,
                               tablearchive,
                               tablerecent){
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
                paste(setdiff(min(years):max(years), years),
                      collapse=","))
    }

    # Remove unnecessary objects
    rm(dtfa)
    rm(dtfr)

    return(dtf)
}


#' Clean Comext Monthly data
#'
#' Loads all trade flows for the given product codes and
#' pass them through the cleaning algorithm:
#' extract prices and conversion factors,
#' check if prices and conversion factors are within bounds,
#' recalculated weight and quantity if needed.
#' Estimate missing quantities.
#'
#' Note: periods are not storred in the priceconversion table,
#' only years are storred in the price and conversion factors tables.
#' years encoded as 2018, will be different to the period 201852
#' @param RMySQLcon database connection object created by RMySQL \code{\link[DBI]{dbConnect}}
#' @param productanalysed character code of the product to analyse
#' @param tablearchive character name of a monthly archive table
#' @param tablerecent character name of a monthly recent table
#' @param tablewrite character name of the monthly table where output data will be written
#' @param tablepriceconversion character name of a table which will store price
#' and conversion factors
#' @return \code{cleancomextmonthly1product} invisibly returns a data frame
#' with all columns generated during the cleaning process.
#' @examples \dontrun{ # Clean product and country codes
#' # Connect to the database
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
#' # Clean product 44079910
#' # clean to the database and also store the invisible output in a data frame
#'
#' dtf <- cleancomextmonthly1product(con ,
#'                            productanalysed = "44071091",
#'                            tablearchive = "raw_comext_monthly_2016S1",
#'                            tablerecent = "raw_comext_monthly_201709",
#'                            tablewrite = "vld_comext_monthly",
#'                            tablepriceconversion = "vld_comext_priceconversion")
#' count(dtf, flag)
#' # Clean all products available in the database
#' cleancomextmonthly(con ,
#'                    tablearchive = "raw_comext_monthly_2016S1",
#'                    tablerecent = "raw_comext_monthly_201709",
#'                    tablewrite = "vld_comext_monthly",
#'                    tablepriceconversion = "vld_comext_priceconversion")
#' # Disconnect from the database
#' RMySQL::dbDisconnect(con)
#' }
#' @export
cleancomextmonthly1product <- function(RMySQLcon,
                                       productanalysed,
                                       tablearchive,
                                       tablerecent,
                                       tablewrite,
                                       tablepriceconversion =
                                           "vld_comext_priceconversion"){
    message("\n\nCleaning product code: ", productanalysed)

    dtf <- loadcomext1product(RMySQLcon = RMySQLcon,
                              productanalysed = productanalysed,
                              tablearchive = tablearchive,
                              tablerecent = tablerecent)

    # Add prices and conversion factors to the data frame
    dtf <- addconversionfactorandprice(dtf)
    # Prepare conversion factors and prices,
    # compute median prices, price bounds and conversion factors in a data frame
    # Edit column names to match comtrade columns
    dtf <- mutate(dtf,
                  # `unit` column hardcoded in estimatequantity()
                  unit = unitcode,
                  # Initiate the flag value to zero as it will be needed later.
                  # The flag value starts at zero
                  # and later flags will be added to each other.
                  flag = 0)

    # Extract prices and conversion factors
    price <- extractprices(dtf, grouping = c("productcode", "flowcode",
                                             "year", "unit"))
    pricew <- extractpricew(dtf, grouping = c("productcode", "flowcode",
                                              "year", "unit"))
    # Join price and pricew in one data frame
    price <- price %>%
        left_join(pricew, by = c("productcode", "flowcode", "year", "unit"))

    conversion <- extractconversionfactors(dtf, grouping = c("productcode", "flowcode",
                                                      "year", "unit"))
    # Join price and conversion factors to the main data frame
    dtf <- joinpricecvfbounds(dtf, price, conversion)

    # Store rows before the change
    nrowbeforechange <- nrow(dtf)


    # Shave pricew based on lower and upper prices added above
    # by the joinpricecvfbounds() function
    dtf <- shavepricew(dtf)
    # Shave conversion based on the lower and upper conversion factors added above
    # by the joinpricecvfbounds() function
    dtf <- shaveconversion(dtf)

    # Estimate quantity
    dtf <- estimatequantity(dtf)


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

    ### Write prices and conversion factors to the database
    # Join price and conversion factors in one data frame
    priceconversion <- price %>%
        left_join(conversion, by = c("productcode", "flowcode", "year", "unit")) %>%
        # Rename unit back to unit code
        rename(unitcode = unit)
    RMySQL::dbWriteTable(con, name = tablepriceconversion,
                         value = priceconversion, append=TRUE, row.names = FALSE)
    return(invisible(dtf))
}


#' @rdname cleancomextmonthly1product
#' @export
cleancomextmonthly <- function(RMySQLcon,
                               productanalysed,
                               tablearchive,
                               tablerecent,
                               tablewrite,
                               tablepriceconversion = "vld_comext_priceconversion",
                               logfile = file.path("~", "comextcleaninglog.txt")){
    # Find all products in the recent and archive table
    #  in the form of a a vector of products available
    dtfr <- tbl(RMySQLcon, tablerecent) %>%
        distinct(productcode) %>% collect()
    dtfa <- tbl(RMySQLcon, tablearchive) %>%
        distinct(productcode) %>% collect()
    # Combine recent and archive products in one vector
    products <- union(dtfr$productcode, dtfa$productcode)

    # Keep only 8 digit product codes
    # two digit products do not have a unit and it doesn't make sense to clean them
    # because they are aggregates of very different distributions
    products <- products[nchar(products)>2]

    # Try to apply the clean function to all products.
    # When cleaning doesn't work for a product, write errors to a log file
    for(productcode in products){
        tryCatch({
            cleancomextmonthly1product(RMySQLcon = RMySQLcon,
                                       productanalysed = productcode,
                                       tablearchive = tablearchive,
                                       tablerecent = tablerecent,
                                       tablewrite = tablewrite,
                                       tablepriceconversion = tablepriceconversion)
        }, error = function(errorcondition){
            write2log(errorcondition, logfile,
                      paste("productcode:", productcode))
        }, warning = function(warningcondition){
            write2log(warningcondition, logfile,
                      paste("productcode:", productcode))
        }
        )
    }
}


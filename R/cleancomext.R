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
                               tablerecent,
                               tableunit = "vld_comext_unit"){
    # load trade flows from the database into a data frame
    message("Load recent data from ", tablerecent, "...")

    # Check if units are available
    unit <- tbl(con, tableunit) %>%
        filter(productcode == productanalysed) %>%
        summarise(n = n()) %>% collect()
    if(unit$n == 0){
        stop("Cannot add units to the table because they are not available in ",
             tableunit)
    }

    dtfr <- tbl(RMySQLcon, tablerecent) %>%
        filter(productcode == productanalysed) %>%
        # Add quantity units
        eutradeflows::addunit2tbl(RMySQLcon,
                                  maintbl = .,
                                  tableunit = tableunit)  %>%
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
                                  tableunit = tableunit)  %>%
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
#' @examples \dontrun{
#' # The cleancomext() function creates its own database connection
#' # It is a high level function meant to be called from a cron job
#' cleancomext(dbname = "test")
#'
#' # If not loaded yet, load the table structure to store validated data
#' eutradeflows::createdbstructure(sqlfile = "vld_comext.sql", dbname = "test")
#'
#' # Connect to the database
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
#' # Create an empty table based on the monthly table template
#' RMySQL::dbSendQuery(con, sprintf("DROP TABLE IF EXISTS `%s`;",
#'                                  "vld_comext_monthly_to_delete"))
#' RMySQL::dbSendQuery(con, sprintf("CREATE TABLE %s LIKE %s;",
#'                                  "vld_comext_monthly_to_delete",
#'                                  "vld_comext_monthly_template"))
#'
#' # \code{cleancomextmonthly1product()} is the main function
#' # calling all other validation functions
#' # Clean to the database and also store the invisible output in a data frame
#' # \code{cleancomextmonthly1product()} will create a copy of the template table.
#' dtf <- cleancomextmonthly1product(con ,
#'                            productanalysed = "44071091",
#'                            tablearchive = "raw_comext_monthly_2016S1",
#'                            tablerecent = "raw_comext_monthly_201708",
#'                            tablewrite = "vld_comext_monthly_to_delete",
#'                            tablepriceconversion = "vld_comext_priceconversion")
#' dplyr::count(dtf, flag)
#' # Drop the temporary table
#' RMySQL::dbSendQuery(con, sprintf("DROP TABLE IF EXISTS `%s`;",
#'                                        "vld_comext_monthly_to_delete"))
#'
#' # Loop on all products available in the database and clean them
#' cleancomextmonthly(con ,
#'                    tablearchive = "raw_comext_monthly_2016S1",
#'                    tablerecent = "raw_comext_monthly_201709",
#'                    tablewrite = "vld_comext_monthly",
#'                    tabletemplate = "vld_comext_monthly_template",
#'                    tablepriceconversion = "vld_comext_priceconversion")
#'
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
    price <- extractprices(dtf,
                           grouping = c("productcode", "flowcode", "year", "unit"),
                           lowercoef = 1, uppercoef = 1,
                           lowerquantile = 0.05, upperquantile = 0.95)
    pricew <- extractpricew(dtf, grouping = c("productcode", "flowcode", "year", "unit"))
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
    message("Beware that `quantityraw` created in estimatequantity() ",
            "may already have been modified ",
            "by the shaveconversion() function.")
    dtf <- estimatequantity(dtf)


    # Before writing prices back to the database, rename some columns
    dtf <- mutate(dtf,
                  unitcode = unit)

    # Use database columns to select which columns to keep in the
    # data frame
    # get column names
    columnswrite  <- RMySQL::dbListFields(RMySQLcon, tablewrite)
    db_dtf <- select_(dtf, .dots = columnswrite)
    # Delete existing data for the given product
    query <- paste("DELETE FROM ", tablewrite,
                   "WHERE productcode = ", productanalysed)
    res <- RMySQL::dbSendQuery(RMySQLcon, query)
    RMySQL::dbClearResult(res)
    message(paste("Writing", nrow(dtf), "flows to the database."))
    # Write dtf
    RMySQL::dbWriteTable(RMySQLcon, name = tablewrite,
                         value = db_dtf, append=TRUE, row.names = FALSE)

    ### Write prices and conversion factors to the database
    # Join price and conversion factors in one data frame
    priceconversion <- price %>%
        left_join(conversion, by = c("productcode", "flowcode", "year", "unit")) %>%
        # Rename unit back to unit code
        rename(unitcode = unit)
    # Delete existing price and conversion factors for the given product
    query <- paste("DELETE FROM ", tablepriceconversion,
                   "WHERE productcode = ", productanalysed)
    res <- RMySQL::dbSendQuery(RMySQLcon, query)
    RMySQL::dbClearResult(res)
    # write price and conversion factors to the database
    RMySQL::dbWriteTable(RMySQLcon, name = tablepriceconversion,
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
                               tabletemplate = "vld_comext_monthly_template",
                               tablepriceconversion = "vld_comext_priceconversion",
                               logfile = file.path("~/log", "cleaningerrorlog.txt")){

    # Create the table that will store validated data
    message("If the database table ", tablewrite,
            " already exists, all its content will be erased and replaced.")
    RMySQL::dbSendQuery(RMySQLcon, sprintf("DROP TABLE IF EXISTS `%s`;",
                                           tablewrite))
    RMySQL::dbSendQuery(RMySQLcon, sprintf("CREATE TABLE %s LIKE %s;",
                                           tablewrite, tabletemplate))

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
            writeerror2log(errorcondition, logfile,
                      paste("productcode:", productcode))
        }, warning = function(warningcondition){
            writeerror2log(warningcondition, logfile,
                      paste("productcode:", productcode))
        }
        )
    }
}


#' @rdname cleancomextmonthly1product
#' @details
#' To run \code{cleancomext} periodically as a cron job, edit crontab:
#'
#' \code{sudo vim /etc/crontab}
#'
#' and enter:
#'
#' \code{
#' 0 5 * * *    paul  Rscript -e "library(tradeflows); cleancomext('tradeflows')" >> ~/log/clean$(date +"\\\%Y\\\%m\\\%d").log 2>&1
#' }
#'
#' During the validation procedure,
#' 3 nested function calls generated 3 log files containing various informations:
#' * The \code{harvest()} function writes a time stamp to
#' \code{~/public_html/log/validate2017.txt}.
#' It is a very summarised, publicly acessible log file.
#' *  The cron instuction redirects standard output to:
#' \code{~/log/clean$(date +"\\\%Y\\\%m\\\%d").log}.
#' It is a very verbose file giving the percentage change of
#' world import and export value after each cleaning operation for each product code.
#' * The \code{cleancomext1product()}function writes errors and warnings to
#' \code{~/log/cleaningerrorlog.txt}.
#' @md
#' @export
cleancomext <- function(dbname,
                        rawtabletemplate = "raw_comext_monthly_template",
                        vldtabletemplate = "vld_comext_monthly_template",
                        tablewrite = "vld_comext_monthly",
                        tablepriceconversion = "vld_comext_priceconversion",
                        templatecharacters = "template",
                        logfile = paste0('~/public_html/log/validate',format(Sys.Date(), '%Y'),'.txt')){
    message("\n\nStarting to clean on ",
            format(Sys.time(),"%Y.%m.%d at %H:%M"),"\n")

    # Connect to the database
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = dbname)

    # List tables
    tables <- RMySQL::dbListTables(con)

    # Clean product, reporter, partner and unit codes
    eutradeflows::cleanallcomextcodes(con)
    message("deal with missing unit in another way not only an error")

    # Extract the name of raw database tables
    rawtablenaked <- gsub(templatecharacters, "", rawtabletemplate)
    recenttables <- grep(paste0(rawtablenaked,"[0-9]{6}"), tables, value = TRUE)
    archivetables <- grep(paste0(rawtablenaked,"[0-9]{4}S1"), tables, value = TRUE)
    # Find the name of the latest "most recent" table
    tablerecent <- sort(recenttables, decreasing = TRUE)[1]
    # Find the name of the latest archive table
    tablearchive <- sort(archivetables, decreasing = TRUE)[1]

    # What is the last period in the most recent raw table?
    raw <- tbl(con, tablerecent) %>%
        summarise(lastperiod = max(period)) %>% collect()

    # What is the last period in the validated table?
    if(tablewrite %in% tables){
        vld <- tbl(con, tablewrite) %>%
            summarise(lastperiod = max(period)) %>% collect()
    } else {
        # If tablewrite (the validated table) doesn't exist in the database
        # vld$lastperiod is NULL
        vld <- list()
    }

    # Compare the last periods between raw and validated data
    if(identical(raw$lastperiod, vld$lastperiod)){
        # If the most recent period is available, just write a message
        message("Data was already validated. ",
                "The last period available in ",
                tablerecent, ": `", raw$lastperiod,
                "` matches the last period available in ",
                tablewrite, ": `", vld$lastperiod,"`.")
        # Add a dot to the main logfile,
        # showing that this function did check for updates in the raw data.
        adddot2logfile(logfile)
    } else {
        # If the most recent period is not available in the validated data
        # clean the dataset again
        write(sprintf("%s\nValidating monthly archives from the %s and %s tables.\n\n",
                      as.character(Sys.time()), tablearchive, tablerecent),
              logfile, append = TRUE)
        cleancomextmonthly(con ,
                           tablearchive = tablearchive,
                           tablerecent = tablerecent,
                           tablewrite = tablewrite,
                           tabletemplate = vldtabletemplate,
                           tablepriceconversion = tablepriceconversion)
    }


    # Disconnect from the database
    RMySQL::dbDisconnect(con)

    message("\nCleaning completed on ", format(Sys.time(),"%Y.%m.%d at %H:%M"))
}

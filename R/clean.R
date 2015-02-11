#' Rename and select columns
#'
#' Rename and select usefull columns for the
#' forest products trade flow database.
#' Renaming is based on a data frame called column_names
#' which is a matching table of column names for the different
#' databases.
#' Rename only columns names that are existing in both
#' sourcedb and destdb.
#' @param dtf data frame
#' @param sourcedb source database, where the data was loaded
#' @param destdb destination database, where the data is going to
#' @import dplyr
#' @examples
#' column_names
#' @export
renamecolumns <- function(dtf, sourcedb = "comtrade", destdb = "efi"){
    column_names <- dplyr::filter(tradeflows::column_names,
                                  !is.na(tradeflows::column_names[sourcedb]) &
                                      !is.na(tradeflows::column_names[destdb]) )
    for (n in column_names[sourcedb][[1]]){
        # TODO: Verify that columns from sourcedb are in the dtf
        names(dtf)[names(dtf)==n] <-
            column_names$efi[column_names[c(sourcedb)]==n]
    }
    destcolumns <- column_names[c(destdb)][[1]]
    dtf <- dplyr::select_(dtf, .dots = destcolumns)
    return(dtf)
}

#' Add regionreporter and regionpartner
#'
#' @param dtf data frame of trade flows
#' @export
addregion <- function(dtf, regioncolumn = "region"){
    dtf %>%
        merge(select(reportercomtrade,
                     reportercode, regionreporter=region), all.x=TRUE) %>%
        merge(select(reportercomtrade,
                     partnercode = reportercode, regionpartner=region), all.x=TRUE)
}


#' Add unit price and conversion factor to each flow
#'
#' Calculate unit prices in current currency
#' Conversion factors in Quantity
#' @param dtf data frame
#' @import dplyr
#' @export
addconversionfactorandprice <- function(dtf){
    dtf %>% mutate(conversion = weight / quantity,
                   price = tradevalue / quantity,
                   # To avoid "integer overflow - use sum(as.numeric(.))" error
                   # on sum of all values
                   tradevalue = as.numeric(tradevalue),
                   # To avoid Error in swd99$c(NA_integer_,  :
                   # invalid subscript type 'integer'
                   quantity = as.numeric(quantity))
}


#' Filter world and EU28
#'
#' Remove EU28 reporter
#' Remove EU-28 and World partner
#' @param dtf data frame containing comtrade trade flows
#' @export
filterworldeu28 <- function(dtf){
     dtf %>%
        filter(!reporter %in% c("EU-28")) %>%
        filter(!partner %in%c("EU-28", "World"))
}


#' Extract median prices at a given geographical aggregation level
#'
#' Extract median prices by region (default) or at another
#' geographical aggregation level (subregion)
#' Prices depend on the quantity unit. For some products quantity unit changes
#' from litre to m3 along the years.
#' @param dtf a dataframe containing already available prices
#' @param geoaggregation a character vector specifying the regional aggregation
#'     level, a column name in the reportercomtrade table
#' @param includeqestimates logical TRUE when comtrade quantity estimates
#' can be included
#' @param lowercoef numeric multiplier of the lower bound on prices
#' @param uppercoef numeric multiplier of the upper bound on prices
#' @export
extractprices <- function(dtf, lowercoef= 0.5, uppercoef=2,
                          geoaggregation="regionreporter",
                          includeqestimates = TRUE){
    # Geoaggregation should be a column of the data frame
    # for example regionreporter
    stopifnot(geoaggregation %in% names(dtf))
    if(includeqestimates==FALSE){ # Condition easier to understand for user of the function
        dtf <- dtf %>% filter(flag==0 |flag==4 )
    }
    dtf %>%
        filter(flow %in% c("Import", "Export")) %>%
        # Remove EU28 reporter
        filter(!reporter %in% c("EU-28")) %>%
        # Remove EU-28 and World partner
        filter(!partner %in%c("EU-28", "World")) %>%
        # Remove missing quantity
        filter(!is.na(quantity)& unit !="No Quantity") %>%
        # Calculate yearly regional prices
        group_by(flow, regionreporter, year, unit) %>%
        summarise(lowerprice = round(lowercoef * quantile(price, 0.25,
                                                    names=FALSE, na.rm=TRUE)),
                  medianprice = round(median(price, na.rm=TRUE)),
                  upperprice = round(uppercoef * quantile(price, 0.75,
                                                  names=FALSE, na.rm=TRUE)),
                  weightedaverageprice = sum()) %>%
        arrange(-medianprice)
}


#' Extract median converion factor at a given geographical aggregation level
#'
#' Extract median converion factors for the whole world (default)
#' @param dtf a dataframe containing conversion factors
#' @param geoaggregation a character string specifying the regional aggregation
#'     level, "world" to extract world conversion factors,
#'      "region" to extract regional conversion factors.
#' @param  includeqwestimates logical TRUE when comtrade quantity and weight
#'          estimates can be included
#' @export
extractconversionfactors <- function(dtf, geoaggregation="region",
                                     includeqwestimates=TRUE){
    if(includeqwestimates==FALSE){
        dtf <- dtf %>% filter(flag==0)
    }
    dtf <- dtf %>%
        filter(flow %in% c("Import", "Export")) %>%
        # Remove EU28 reporter
        filter(!reporter %in% c("EU-28")) %>%
        # Remove World partner
        filter(!partner %in%c("EU-28", "World")) %>%
        # Remove missing quantity
        filter(!is.na(quantity) & unit !="No Quantity")
    if(geoaggregation == "region"){
        dtf <- dtf %>%  group_by(flow, regionreporter, year, unit)
    } else {
        dtf <- dtf %>% group_by(flow, year, unit)
    }
    dtf %>%
        summarise(medianconversion = round(median(conversion,na.rm=TRUE)))
}


#' Estimate quantity
#'
#' Estimate missing quantity
#' For each trade flow in the given data frame,
#' compute quantity_cf from the weight using a conversion factor and
#' comput quantity_up from the trade value using a unit price.
#' These columns will be available in the data frame output of this function,
#' but they will not be saved in the validated database.
#' Then split the data frame
#' complete missing quantity from weight when available or from the
#' tradevalue when the weight is not available
#' Check unit price
#' Check unit price of the trade flow, against a table of reference unit prices
#' Check upper and lower bounds on price.
#' In general NA values should be avoided for the
#'  upper and lower bounds on prices
#' @param dtf data frame
#' @param price a data frame of unit prices which can be merge with dtf
#' @param conversionfactor a data frame of conversion factors which can be
#'      merged with dtf
#' @export
estimatequantity <- function(dtf, price, conversionfactor){
    # check if at least 4 common columns are present in dtf
    # for the merge with price (Flow, regionreporter, period, unit)
    stopifnot(sum(names(price) %in% names(dtf))>=4)
    # Check if at leat 3 common columns are present with dtf
    # for the merge with conversionfactor
    stopifnot(sum(names(conversionfactor) %in% names(dtf))>=3)

    ### Specify quantity unit
    # Extract the unit most present in the price data frame
    unitprefered <-  price %>% group_by(unit) %>% summarise(n=n())
    unitprefered <- unitprefered$unit[unitprefered$n==max(unitprefered$n)]
    # Replace unit "No Quantity" and unit NA by the most prefered unit
    # before merging price and conversion factor tables
    dtf$unit[dtf$unit=="No Quantity"] <- unitprefered
    dtf$unit[is.na(dtf$unit)] <- unitprefered

    ### For all flows
    # Keep raw quantity for further analysis
    dtf <- dtf %>% mutate(quantityraw = quantity)
    # Estimate quantity based on the weight using a conversion factor
    dtf <- merge(dtf, conversionfactor, all.x=TRUE) %>%
        mutate(quantity_cf = weight / medianconversion)
    # Estimate quantity based on the trade value, using a unit price
    dtf <- merge(dtf, price, all.x=TRUE) %>%
        mutate(quantity_up = tradevalue / medianprice,
               havequantity = !is.na(quantity))

    ### Split flows which have a quantity from those which don't
    # Replace quantity by quantity_cf or quantity_up
    dtfq <- dtf %>% filter(!is.na(quantity))
    dtfnoqw <- dtf %>% filter(is.na(quantity) & !is.na(weight)) %>%
        mutate(quantity = quantity_cf,
               flag = flag + 10)
    dtfnoqnow <- dtf %>% filter(is.na(quantity) & is.na(weight)) %>%
        mutate(quantity = quantity_up,
               flag = flag + 20)
    stopifnot(nrow(dtf)==nrow(dtfq) + nrow(dtfnoqw) + nrow(dtfnoqnow))
    # Messages about the number of rows affected
    nrow(dtf) %>% message(" rows in the dataset")
    nrow(dtfnoqw) %>% message(" rows where quantity was not available but weight was available")
    nrow(dtfnoqnow) %>% message(" rows where neither quantity nor weight were available")
    # Put data frames back together
    dtf <- rbind(dtfq, dtfnoqw, dtfnoqnow)
    return(dtf)
}


#' Compare reporter and partner flow
#'
#' The data frame outcome of this function
#' will be use by replacebypartnerquantity()
#' to decide which of the reporter or partner flow to favor
#' @param dtf data frame
#' @param periodbegin change this to global parameter
#' @param periodend change this to global parameter
choosereporterorpartner <- function(dtf,
                                    periodbegin=2009, periodend=2013,
                                    sdratiolimit = 0.8){
    choice <- dtf %>%
        filter(periodbegin <= period & period<= periodend) %>%
        mutate(pricereporter = tradevalue / quantityreporter,
               pricepartner = tradevaluepartner / quantitypartner) %>%
        group_by(flow, reportercode, partnercode,
                 reporter, partner) %>%
        summarise(
            ### Reporter quantity q and price p
            meanqreporter = mean(quantityreporter, na.rm=TRUE),
            meanpreporter = mean(pricereporter, na.rm=TRUE),
            sdpreporter = sd(pricereporter, na.rm=TRUE),
            ### Partner price p
            meanppartner = mean(pricepartner, na.rm=TRUE),
            sdppartner = sd(pricepartner, na.rm=TRUE),
            ### Ratio of the standard deviation on price
            sdratio = sdppartner / sdpreporter,
            favorpartner = sdratio < sdratiolimit)
    return(choice)
}


#' Replace quantity by quantity partner when the price variation in
#' recent periods was greater than that of the partner country for the same flow
#'
#' @param dtf data frame
#' @param choice a data frame of choice between reporter and partner
replacebypartnerquantity <- function(dtf, choice){
    choice <- choice %>% select(reportercode, partnercode, favorpartner)
    dtf <- merge(dtf, choice, all.x=TRUE)
    # cut the dataframe between the lines which favor partner and the others
    dtffavor <- dtf %>% filter(favorpartner) %>%
        mutate(quantity = quantitypartner,
               flag = flag + 300)
    dtfrest <- dtf %>% filter(!favorpartner | is.na(favorpartner))
    stopifnot(nrow(dtf) == nrow(dtffavor) + nrow(dtfrest))
    message(nrow(dtffavor), "rows where quantity reporter was replaced by quantity partner")
    dtf <- rbind(dtffavor, dtfrest)
    return(dtf)
}


#' Change quantities when unit prices are too high or too low
#'
#' Upper and lower bound on prices have to be
#' checked and quantity replaced accordingly
#' This function comes after estimatequantity()
#' and after addpartnerflow()
#' check this order in the source of the clean() function.
#' Check unit price for all flows
#' recalculate unit price based on quantity estimate
#' Those which have been estimated from tradevalue will have correct prices
#' those which have been estimated from weight might be changed as well
#' @param dtf data frame
#' @export
shaveprice <- function(dtf){
    # Split flows which have prices out of bounds from those which don't
    dtf <- dtf %>% mutate(rawprice = price,
                          price = tradevalue / quantity)
    dtfinbound <- dtf %>%
        filter(lowerprice<=price & price<=upperprice|
                   is.na(price) |
                   # These conditions are problematic
                   # In general NA values should be avoided for the
                   # upper and lower bounds on prices
                   is.na(lowerprice)|is.na(upperprice))
    dtfoutbound <- dtf %>%
        filter(price<lowerprice | upperprice<price) %>%
        mutate(quantity = quantity_up,
               flag = flag + 4000)
    stopifnot(nrow(dtf) == nrow(dtfinbound) + nrow(dtfoutbound))
    message(nrow(dtfoutbound), " rows had a price too high or too low")
    dtf <- rbind(dtfinbound, dtfoutbound)
    return(dtf)
}


#' Change some of the column types
#'
#'  Change to factors for easy plotting
#'  Change to large int or to floating point for some computations
#' @param dtf data frame
#' @export
changecolumntype <- function(dtf){
    dtf$flag <- as.factor(dtf$flag)
    return(dtf)
}


#' Return a dataframe of duplicated flows
#'
#' @param dtf a dataframe contiaining trade flows, the funciton
#'  tests for duplicated reportercode, partnercode, productcode, flow, period
#' @export
findduplicatedflows <- function(dtf){
    # This checks only the selected columns in dtf
    dtf$duplicate <- duplicated(select(dtf, reportercode, partnercode,
                                       productcode, flow, period))
    filter(dtf, duplicate)
}


#' Remove duplicated flows
#'
#' Print the number of duplicated entries and remove them if needed.
#' The function tests for all duplicated columns
#' (contrary to findduplicatedflows() which looks only for some columns).
#' This might be an issue if 2 flows have duplicated
#' reportercode, partnercode, productcode, flow, period but
#' their quantity column is different.
#' @param dtf a dataframe contiaining trade flows,
#' @export
removeduplicatedflows <- function(dtf){
    if(nrow(findduplicatedflows(dtf))>0){
        message("There were duplicated lines for the following reporters:")
        message(unique(findduplicatedflows(dtf)$reporter))
        # This checks for all columns in dtf
        return(unique(dtf))
    }
    return(dtf)
}



#' Add volume and value of the partner flow
#'
#' For the same country, period, item, add the corresponding partner flow.
#'@param dtf data frame containing trade flow data.
#'   With column names following the efi convention.
#'@export
addpartnerflow <- function(dtf){
    # Warning for duplicated entries
    if(nrow(findduplicatedflows(dtf))>0){
        stop("Remove duplicated entries before adding partner flows")
    }
    swap <- dtf %>%
        rename(partnercode = reportercode,
               reportercode = partnercode) %>%
        select(reportercode, partnercode,
               productcode, flow, period,
               quantity, tradevalue) %>% #Add weight, tradevalue here if you want them from the partner
        mutate(flow = gsub("Import","aaaaaaa",flow),
               flow = gsub("Export","Import",flow),
               flow = gsub("aaaaaaa","Export",flow))
    # Check that values in the dtf and swap tables are all there
    # and in the same order (remove NA values from the check)
    stopifnot(dtf$quantity[!is.na(dtf$quantity)] ==
                  swap$quantity[!is.na(swap$quantity)])
    # Merge
    dtf <- merge(dtf, swap, all.x=TRUE, suffixes = c("", "partner"),
                 by = c("reportercode", "partnercode",
                        "productcode", "flow", "period"))
    dtf$quantityreporter <- dtf$quantity
    return(dtf)
}


#' Calculate discrepancies
#'
#' The plan is to use this as needed before plotting
#' but not to store it in the enddata
#' as it is for plotting with ggplot,
#' it might work best on a reshaped data frame
#'
#'@param dtf data frame reshaped in long format.
#'   containing trade flow data.
#'   With column names following the efi convention.
#' @export
calculatediscrepancies <- function(dtf){
    require(reshape2)
    # Reshape data frame in long format before calculation
    # Reshape in wide format before returning output dtf
    #     ids <- names(dtf)[!names(dtf) %in%
    #                           c("weight", "quantity", "tradevalue",
    #                             "weightpartner", "quantitypartner",
    #                             "tradevaluepartner")]
    #     stopifnot(length(ids) + 6 == length(names(dtf)))
    dtf %>% #melt(dtf, id=ids)
        mutate(discrw = weightpartner - weight,
               discrq = quantitypartner - quantity,
               discrv = tradevaluepartner - tradevalue,
               reldiscrw = signif((weightpartner - weight)/
                                          (weight + weightpartner),2),
               reldiscrq = signif((quantitypartner - quantity)/
                                            (quantity + quantitypartner),2),
               reldiscrv = signif((tradevalue - tradevaluepartner)/
                                            (tradevalue + tradevaluepartner),2))
}


#' Combine cleaning functions
#'
#' Use unit prices and conversion factors to complete missing quantity data.
#' Handle unit prices that are out of bound.
#' Add quantity estimates reported by the trade partner.
#' The outcome is returned as a data frame.
#' @param dtf data frame
#' @param replacebypartnerquantity, when TRUE call replacebypartnerquantity()
#' @param shaveprice when TRUE, call shaveprice()
#' @param deleteextracolumns when TRUE, keep only columns from
#' tradeflows::column_names$validated_flow
#' @param outputalltables when TRUE return a list of data frames
#' containing the cleaned dataset and all tables used to compute it
#' conversion factors, regional unit prices, choice between
#' reporter and partner flows
#' @return data frame with the same information as the original
#' data frame. With columnes added by the various clean functions.
#' @export
clean <- function(dtf,
                  geoaggregation = "region",
                  replacebypartnerquantity = TRUE,
                  shaveprice = TRUE,
                  outputalltables = FALSE){
    nrowbeforechange <- nrow(dtf)

    ### Prepare conversion factors and prices
    dtf <- dtf %>%
        removeduplicatedflows %>%
        addconversionfactorandprice %>%
        addregion
    price <- extractprices(dtf)
    conversionfactor <- extractconversionfactors(dtf, geoaggregation = geoaggregation)

    ### Estimate quantity
    dtf <- dtf %>%
        estimatequantity(price, conversionfactor) %>%
        addpartnerflow
    if (replacebypartnerquantity){
        choice <- choosereporterorpartner(dtf,sdratiolimit = 1 )
        dtf <- dtf %>% replacebypartnerquantity(choice)
    }
    if (shaveprice){
        dtf <- dtf %>% shaveprice # based on upper and lower prices added above
    }

    # Check if the number of rows has changed (it shouldn't)
    # It might change if there are duplicated flows
    stopifnot(nrow(dtf) == nrowbeforechange)

    ### 2 different kinds of output
    # List output
    if(outputalltables){
        return(list(dtf = dtf,
                    price = price,
                    conversionfactor = conversionfactor,
                    choice = choosereporterorpartner(dtf))
        )
    }
    # Data frame output
    return(dtf)
}

#' Clean monthly trade flows
#'
#' The function uses regional conversion factors
#' and unit prices generated from the yearly data frame.
#' @param dtfmonthly data frame of monthly trade flows
#' @export
cleanmonthly <- function(dtfmonthly,
                         dtfyearly,
                         geoaggregation = "region",
                         replacebypartnerquantity = TRUE,
                         shaveprice = TRUE,
                         outputalltables = FALSE){
    ### Prepare yearly conversion factors and prices
    y <- clean(dtfyearly, geoaggregation = geoaggregation, outputalltables = TRUE)

    ### Prepare monthly data
    # Keep only columns usefull for R,
    # Those efi column names that are in config/column_names.csv
    columnsread <- names(dtfmonthly)[names(dtfmonthly) %in%
                                   column_names$efi[column_names[,"raw_flow_monthly"]]]
    dtfmonthly <- dtfmonthly %>%
        select_(.dots= columnsread) %>%
        removeduplicatedflows %>%
        #     no quantity means no conversion factor and price
        #     addconversionfactorandprice %>%
        addregion
    nrowbeforechange <- nrow(dtfmonthly)

    ### Estimate quantity
    # Replace "exports" and "import" by "export" and "import"
    dtfmonthly$flow <-gsub("ports", "port", dtfmonthly$flow)
    dtfmonthly <- dtfmonthly %>%
        estimatequantity(y$price, y$conversionfactor) %>%
        addpartnerflow
    if (replacebypartnerquantity){
        dtfmonthly <- dtfmonthly %>% replacebypartnerquantity(y$choice)
    }
    if (shaveprice){
        dtfmonthly <- dtfmonthly %>%
            mutate(price = tradevalue / quantity) %>% # Recaculate  price
            shaveprice # based on yearly upper and lower prices added above
    }

    # Check if the number of rows has changed (it shouldn't)
    # It might change if there are duplicated flows
    stopifnot(nrow(dtfmonthly) == nrowbeforechange)

    ### 2 different kinds of output
    # List output
    if(outputalltables){
        return(list(dtf = dtfmonthly,
                    price = y$price,
                    conversionfactor = y$conversionfactor,
                    choice = y$choice)
        )
    }
    # Data frame output
    return(dtfmonthly)
}

#' Write flows into the database table validated_flow
#'
#' For one product (at the 6 digit level),
#' read and clean all trade flows, then
#' Write flows into the database table(s) validated_flow
#' updates will be done on a product basis,
#' The function will:
#' \enumerate{
#'  \item{Read all flows having the given productcode in tableread}
#'  \item{Delete all flows having the given productcode
#'  in tablewrite (between all reporter and partner countries in all years)}
#'  \item{Use  \code{\link{clean}()} to clean the data frame}
#'  \item{Write validated flows to tablewrite}
#' }
#' @return TRUE if write to db succesded, otherwise return FALSE
#' @param productcode_ code of the product trade flows to be validated
#' @param tableread name of the table to read from
#' @param tablewrite name of the table to write to (all rows for productcode
#' will be deleted in this table before writing)
#' @param ... further arguments passed to the clean function
#' @export
cleandbproduct <- function(productcode, tableread, tablewrite, ...){
    checkdbcolumns(c(tableread, tablewrite))
    dtf <- readdbproduct(productcode, tableread = tableread)
    ### Remove database specific columns keep only columns usefull for R
    # Those efi column names that are in config/column_names.csv
    columnsread <- names(dtf)[names(dtf) %in%
                                     column_names$efi[column_names[,tableread]]]
    dtf <- dtf %>% select_(.dots= columnsread)
    dtf <- removeduplicatedflows(dtf)
    dtf <- clean(dtf, ...)
    # Remove column names added by the merge with price and conversionfactor tables
    # Keep only column names in the final table validated_flow
    # This is usefull for database output
    columnswrite <- column_names$efi[column_names[,tablewrite]]
    # Add or remove colum names here if needed
    # Remove lastchanged because is in the database
    # raw_flow table but we want the database to compute it again
    # for the validated flow
    columnswrite <- columnswrite[!columnswrite == "lastchanged"]
    dtf <- dtf %>% select_(.dots = columnswrite)
    deletedbproduct(productcode, tablewrite)
    message(paste("writing", nrow(dtf), "flows to the database"))
    # Message concerning the database write
    dbmessage<- data.frame(result = c(TRUE, FALSE),
                           message =  c("Write to the database succeeded",
                                        "Write to the database failed"))
    result <- FALSE
    tryCatch(result <- writedbproduct(dtf, tablewrite),
             finally = message(dbmessage$message[dbmessage$result == result]))
}


if(FALSE){
    library(dplyr)
    load("data-raw/comtrade/sawnwood_all.RData")
    # Step by step
    sawnwood <- swdall %>%
        renamecolumns %>%
        removeduplicates %>%
        addpartnerflow %>%
        calculatediscrepancies
    # All in one step, except for renamecolumns because
    # it's specific to the development system
    # In the production system, columns will already have been renamed
    sawnwood <- swdall %>%
        renamecolumns() %>%
        clean()
    save(sawnwood, file="data/sawnwood.RData")
}


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


#' Check various features of the trade flows data frame
#'
#' "Import" and "Export" and used to add partner flow information.
#' Should these characters be different then the user should be warned.
#' "Import" and "Export" are hardcoded in the swapreporterpartner() function.
#' This should be changed to environement variables using options() and getOption().
#' @param dtf data frame of trade flows data
#' @export
sanitycheck <- function(dtf){
    # Check that flows are written with a firt uppercase later
    flow <- unique(dtf$flow)
    stopifnot(sum(grepl("Import",flow) + grepl("Export",flow)) ==
                  length(flow))
}


#' Return the length of unique combination of given column names
#' @param dtf data frame
#' @param ... further arguments passed to \code{\link{select_}()}
#' @examples
#' lengthunique(airquality)
#' lengthunique(airquality, "Month", "Day")
#' lengthunique(airquality, "Month")
#' lengthunique(airquality, "Day")
#' @export
lengthunique <- function(dtf, ...){
    dtf <- dtf %>%
        select_(...)
    if(ncol(dtf)!=0){
        dtf <- dtf %>% distinct()
    }
    return(nrow(dtf))
}



#' Add regionreporter and regionpartner
#'
#' @param dtf data frame of trade flows
#' @param regioncolumn column containing region names
#' @export
addregion <- function(dtf, regioncolumn = "region"){
    dtf %>%
        merge(select(reportercomtrade,
                     reportercode, regionreporter = region), all.x=TRUE) %>%
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
                   # To avoid Error in dtf$c(NA_integer_,  :
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
#' Group trade flows by the given geographical aggregation level
#' (region or subregion) and extract the median prices.
#' Prices depend on the quantity unit. For some products quantity unit changes
#' from litre to m3 along the years.
#' @param dtf a dataframe containing all trade flows for one product
#' and their individual prices
#' @param grouping a character vector specifying the grouping variables
#' @param includeqestimates logical TRUE when comtrade quantity estimates
#' can be included
#' @param lowercoef numeric multiplier of the first quartile to
#' obtain a lower bound on prices
#' @param uppercoef numeric multiplier of the third quartile to
#' obtain an upper bound on prices
#' @export
#' @examples
#' \dontrun{
#' # tf a data frame of trade flows
#' price <-  tf %>% extractprices()
#' priceglobal <- tf %>%
#'     extractprices(grouping = c("flow", "year", "unit"))
#' }
##'
extractprices <- function(dtf, lowercoef= 0.5, uppercoef=2,
                          grouping = c("flow", "regionreporter", "year", "unit"),
                          includeqestimates = TRUE){
    # Grouping variables should be present in the data frame
    stopifnot(grouping %in% names(dtf))
    # Price should be present in the data frame
    stopifnot("price" %in% names(dtf))
    if(identical(includeqestimates,FALSE)){ # Condition easier to understand for user of the function
        dtf <- dtf %>% filter(flag==0 |flag==4 )
    }
    # replace infinite price values by NA for the mean price
    # calculation
    dtf$price[is.infinite(dtf$price)] <- NA
    dtf %>%
        filter(flow %in% c("Import", "Export")) %>%
        filterworldeu28() %>%
        # Price whould not be NA and not be infinite
        filter(!is.na(price) & !is.infinite(price)) %>%
        # Calculate yearly regional prices by unit
        group_by_(.dots = grouping) %>%
        summarise(lowerprice = lowercoef * quantile(price, 0.25, names=FALSE),
                  medianprice = median(price),
                  upperprice = uppercoef * quantile(price, 0.75, names=FALSE),
                  # The average price often cannot be computed because there
                  # are infinite prices when quantity is = 0
                  averageprice = mean(price, na.rm=TRUE),
                  weightedaverageprice = sum(tradevalue)/ sum(quantity) #,
                  # Price weighted by the quantity, same value as above
                  # weightedaverageprice1 = sum(price * quantity, na.rm=TRUE)/
                  #  sum(quantity, na.rm=TRUE),
                  # Price weighted by the tradevalue
                  # Infinite prices are an issue to calculate
                  # sum(price * tradevalue, na.rm=TRUE)
                  # One could replace Inf by NA values
                  # weightedaverageprice2 = sum(price * tradevalue, na.rm=TRUE)/
                  #  sum(tradevalue, na.rm=TRUE),
                  ) %>%
        arrange(desc(medianprice))
}


#' Extract median conversion factor at a given geographical aggregation level
#'
#' Group trade flows by the given geographical aggregation level and
#' extract the median conversion factors.
#' @param dtf a dataframe containing all trade flows for one product
#' and their individual conversion factors
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
        filterworldeu28() %>%
        # Remove missing and infinite conversion factors
        filter(!is.na(conversion) & !is.infinite(conversion) &
                   !conversion == 0)

    if(geoaggregation == "region"){
        dtf <- dtf %>%  group_by(flow, regionreporter, year, unit)
    } else {
        dtf <- dtf %>% group_by(flow, year, unit)
    }
    dtf %>%
        summarise(medianconversion = round(median(conversion)))
}


#' A function to calculate the percentage change on world imports and exports
#' @param dtf0 a data frame of trade flows before the modification
#' @param dtf1 a data frame of modified trade flows
#'
#' Note: Add a function changeflowmessage(dtf, dtfsplit) which will return a character
#' string about both trade flows. This function could be defined in the main scope
#' and reused in other parts of the workflow. Or maybe not because the quantity is
#' not empty for addpartner flow and shaveprice.
#' @export
changeflowmessage <- function(dtf0, dtf1){
    sumflow <- function(flow){
        sum0 <- sum(dtf0$quantity[dtf0$flow == flow], na.rm=TRUE)
        sum1 <- sum(dtf1$quantity[dtf1$flow == flow], na.rm=TRUE)
        signif(((sum1 - sum0) / sum0) * 100, 2)
    }
    sentence <- paste("changed world exports by", sumflow("Export"),"%",
                      "and world imports by", sumflow("Import"), "%",
                      " (positive values imply an increase).")
    return(sentence)
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
    # Arrange by descending order of last change
    # So that the duplicates returned by this function are the old ones
    dtf <- dtf %>% arrange(desc(lastchanged))
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
    duplicatedflows <- findduplicatedflows(dtf)
    if(nrow(duplicatedflows)>0){
        message("There were duplicated lines for the following reporters:")
        message(unique(duplicatedflows$reporter))
        # Remove duplicated columns in dtf
        return(anti_join(dtf, duplicatedflows,
                         by=c("reportercode", "partnercode", "productcode",
                              "flow", "period", "lastchanged")))
    }
    return(dtf)
}


#' Rename reporter to partner and partner to reporter
#' @param dtf data frame containing trade flow data
#' @param column column names to select, NULL if all columns have to be selected
#' Selected column names should at least include the identity columns which will be used
#' used for the merge
#' @export
swapreporterpartner <- function(dtf, column=c("reportercode", "partnercode","productcode",
                                              "flow","period","quantity","tradevalue")){
    swap <- dtf %>%
        rename(partnercode = reportercode,
               reportercode = partnercode,
               partner = reporter,
               reporter = partner,
               partneriso = reporteriso,
               reporteriso = partneriso) %>%
        mutate(flow = gsub("Import","aaaaaaa",flow),
               flow = gsub("Export","Import",flow),
               flow = gsub("aaaaaaa","Export",flow))
    if (!is.null(column)){
        swap <- swap %>%
            select_(.dots = column)
    }
    return(swap)
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
    swap <- dtf %>% swapreporterpartner()
    # Check that values in the dtf and swap tables are all there
    # and in the same order (remove NA values from the check)
    stopifnot(dtf$quantity[!is.na(dtf$quantity)] ==
                  swap$quantity[!is.na(swap$quantity)])
    # Add quantity_partner and tradevalue_partner to existing flows
    dtf <- merge(dtf, swap, all.x=TRUE, suffixes = c("", "partner"),
                 by = c("reportercode", "partnercode",
                        "productcode", "flow", "period"))
    # For flows that are missing add
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
        mutate(discrq = quantitypartner - quantity,
               discrv = tradevaluepartner - tradevalue,
               reldiscrq = signif((quantitypartner - quantity)/
                                            (quantity + quantitypartner),2),
               reldiscrv = signif((tradevalue - tradevaluepartner)/
                                            (tradevalue + tradevaluepartner),2))
}

#' Estimate quantity
#'
#' Estimate missing quantity
#' For each trade flow in the given data frame,
#' compute quantity_cf from the weight using a conversion factor and
#' comput quantity_up from the trade value using a unit price.
#' These columns will be available in the data frame output of this function,
#' but they will not be saved in the validated database.
#'
#' Split the data frame between flows which have a quantity from those which don't
#' complete missing quantity from weight when available or from the
#' tradevalue when the weight is not available
#' Check upper and lower bounds on price against a table of reference unit prices
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

    # Split flows which have a quantity from those which don't
    dtfq <- dtf %>% filter(!is.na(quantity))
    dtfnoqw <- dtf %>% filter(is.na(quantity) & !is.na(weight) &
    # If median conversion factor is NA,
    # then estimate quantity based on price
                                  !is.na(medianconversion))
    dtfnoqnow <- dtf %>% filter(is.na(quantity) &
                                    (is.na(weight) | is.na(medianconversion)))
    # Replace quantity by quantity_cf or by quantity_up
    dtfnoqw <- dtfnoqw %>% mutate(quantity = quantity_cf,
                                  flag = flag + 10)
    dtfnoqnow <- dtfnoqnow %>% mutate(quantity = quantity_up,
                                      flag = flag + 20)
    stopifnot(nrow(dtf)==nrow(dtfq) + nrow(dtfnoqw) + nrow(dtfnoqnow))
    # Messages about the number of rows affected
    nrow(dtf) %>% message(" rows in the dataset")
    nrow(dtfnoqw) %>% message(" rows where quantity was not available but weight was available")
    message("Using a conversion factor to estimate quantity from weight ",
            changeflowmessage(dtf,rbind(dtf,dtfnoqw)))
    nrow(dtfnoqnow) %>% message(" rows where neither quantity nor weight were available")
    message("Using a unit price to estimate quantity from weight ",
            changeflowmessage(dtf,rbind(dtf,dtfnoqnow)))

    # Put data frames back together
    dtf <- rbind(dtfq, dtfnoqw, dtfnoqnow)
    return(dtf)
}


#' Compare reporter and partner flow
#'
#' The data frame outcome of this function
#' will be use by replacebypartnerquantity()
#' to decide which of the reporter or partner flow to favor
#'
#' NA values should not be present in prices.
#' If an NA value is present in one year,
#' it should be probagated to all other years.
#' That means do not use na.rm in the calculation of the
#' standard deviation of prices.
#'
#' Because of missing values in more recent years, the presence of missing data towards the end
#' can be dealt with by choosing periodend 2 or 3 years before the last year available.
#' @param dtf data frame
#' @param periodbegin change this to global parameter
#' @param periodend change this to global parameter
choosereporterorpartner <- function(dtf,
                                    periodbegin=2009, periodend=2013,
                                    sdratiolimit = 0.8,
                                    verbose = getOption("tradeflows.verbose",TRUE)){
    if(verbose){
        message(periodbegin, "and", periodend, "used for the begining and end period")
    }
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
            sdpreporter = sd(pricereporter),
            ### Partner price p
            meanppartner = mean(pricepartner, na.rm=TRUE),
            sdppartner = sd(pricepartner),
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
replacebypartnerquantity <- function(dtf, choice, verbose = getOption("tradeflows.verbose",TRUE)){
    choice <- choice %>%
        select(reportercode, partnercode, favorpartner, flow)
    dtf <- merge(dtf, choice, all.x=TRUE) %>%
        # add again the partner data, because it can have been modified in between
        # remove these columns, they will be recreated by addpartnerflow
        select(-tradevaluepartner, -quantitypartner) %>%
        addpartnerflow()

    # cut the dataframe between the lines which favor partner and the others
    dtffavor <- dtf %>%
        filter(favorpartner & !is.na(quantitypartner)) %>%
        mutate(quantity = quantitypartner,
               flag = flag + 4000)
    dtfrest <- dtf %>% filter(!favorpartner | is.na(favorpartner) | is.na(quantitypartner))
    dtfresult <- rbind(dtffavor, dtfrest)
    stopifnot(nrow(dtf) == nrow(dtfresult))
    if(verbose){
        message(nrow(dtffavor), " rows where quantity reporter was replaced by quantity partner")
        message("Favouring the mirror flow ",
                changeflowmessage(dtf,dtfresult))
    }
    return(dtfresult)
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
#' Those which have been estimated from weight might be changed as well
#' Split trade flows between those which are within the price bounds
#' and those which are out of the price bounds.
#' For trade flows outside the price bounds,
#' use the unit price to estimate the quantity
#' @param dtf data frame
#' @export
shaveprice <- function(dtf, verbose = getOption("tradeflows.verbose",TRUE)){
    # Split flows which have prices out of bounds from those which don't
    dtf <- dtf %>% mutate(rawprice = price,
                          price = tradevalue / quantity)
    # Deal with missing prices and missing price bounds
    dtfnoboundprice <- dtf %>%
        # These NA conditions on lowerprice and upperprice are problematic
        # In general NA values should be avoided for the
        # upper and lower bounds on prices
        # The price calculation should add
        # missing upper and lower bound from the world price bounds
        # And this dtfnoboundprice data frame should be empty
        filter((is.na(lowerprice)|is.na(upperprice)) & !is.na(price))
    dtfnoprice <- dtf %>%
        filter(is.na(price))
    if (identical(nrow(data.frame()),0L)){
        # it is not possible to check this in the price calculation
        # because some NA values can come from the merge
        message(nrow(dtfnoboundprice), " rows where price bounds were not available")
    }
    dtfinbound <- dtf %>%
        filter((lowerprice <= price & price <= upperprice))
    # This is where the modification took place
    dtfoutbound <- dtf %>%
        # Will contain also Inf prices
        filter(price<lowerprice | upperprice<price) %>%
        mutate(quantity = quantity_up,
               flag = flag + 300)
    dtfresult <- rbind(dtfinbound, dtfoutbound, dtfnoboundprice, dtfnoprice)
    stopifnot(nrow(dtf) == nrow(dtfresult))
    if (verbose){
        message(nrow(dtfoutbound), " rows had a price too high or too low")
        message("Readjusting quantities so that prices are within the lower and upper bounds",
                changeflowmessage(dtf, dtfresult))
    }
    return(dtfresult)
}


#' Add the partner flow when quantity is missing
#'
#' When a flow does not have a mirror flow.
#' @param dtf a data frame containing world trade flows for one product
#' @export
addmissingmirrorflow <- function(dtf){
    # Remove world and EU 28 as it doesn't make sense to
    # add a partner flow for these

    # Select only the id columns from dtf
    dtfid <- dtf %>%
        select(reportercode, partnercode,
               productcode, flow, period)
    # Swap all columns from dtf
    swap <- dtf %>%
        swapreporterpartner(column=NULL)
    # keep those flows that do not have a mirror in dtf
    dtfantijoin <- anti_join(swap,dtfid,
                             by = c("period", "flow",
                                    "partnercode", "reportercode",
                                    "productcode")) %>%
        # Change flag to 5000
        mutate(flag = flag + 5000)
    # Add these to the original table
    dtfresult <- rbind(dtf, dtfantijoin)
    message(nrow(dtfantijoin), " rows didn't have a mirror flow.")
    message("Adding missing mirror flows ",
            changeflowmessage(dtf, dtfresult))
    return(dtfresult)
    # findduplicatedflows(dtfresult)
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
#' column_names$validated_flow
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
                  addmissingmirrorflow = TRUE,
                  outputalltables = FALSE,
                  includeqestimates = TRUE){

    ### Checks
    dtf %>% sanitycheck()

    ### Prepare the data frame
    dtf <- dtf %>%
        filterworldeu28()  %>%
        removeduplicatedflows() %>%
        addconversionfactorandprice() %>%
        addregion

    ### Prepare conversion factors and prices
    price <- extractprices(dtf, includeqestimates)
    conversionfactor <- extractconversionfactors(dtf, geoaggregation = geoaggregation)

    ### Estimate quantity
    nrowbeforechange <- nrow(dtf)
    dtf <- dtf %>%
        estimatequantity(price, conversionfactor) %>%
        addpartnerflow
    # Shave price has to be done before replacebypartnerquantity, if the flow is mirrored
    # the trade value is not relevant anymore because it represents the and not the
    if (shaveprice){
        # based on upper and lower prices added above
        # by the estimatequantity() function
        dtf <- dtf %>% shaveprice()
    }
    if (replacebypartnerquantity){
        choice <- choosereporterorpartner(dtf,sdratiolimit = 1 )
        dtf <- dtf %>% replacebypartnerquantity(choice)
    }
    # Check if the number of rows has changed (it shouldn't)
    # It might change if there are duplicated flows
    stopifnot(nrow(dtf) == nrowbeforechange)
    # Now the numer of rows will change
    if(addmissingmirrorflow){
        dtf <- dtf %>% addmissingmirrorflow()
    }

    ### Overwrite partner quantity with the new estimated quantity
    # so that reporterquantity and partnerquantity are consistent
    dtf <- dtf %>% select(-quantitypartner, -tradevaluepartner, -quantityreporter) %>%
        # Create a function suppresspartnerflow that would be the complement of addpartnerflow
        addpartnerflow()

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
    message("\nIn an ideal world conversion factors, prices and choice table would be placed
in a database table, and not recalculated each time from the raw_flow_yearly.
We sacrificed a few seconds of execution time for an easier implementation.\n")
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
#' @param productcode code of the product trade flows to be validated
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
    message("deleteting product code ", productcode, " from the table ", tablewrite)
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
    library()
    ### Clean from one database table, to another database table
    cleandbproduct(440799, "raw_flow_yearly", "validated_flow_yearly")

    ### clean from a file
    load("data-raw/comtrade/440799.RData")
    # All in the clean() function,
    # except for renamecolumns() because
    # it's specific to the development system
    # In the production system,
    # database columns will already have been renamed
    sawnwood <- dtf %>%
        renamecolumns() %>%
        clean()
    save(sawnwood, file=file.path(tempdir(),"sawnwood.RData"))
    message("File saved to ",file.path(tempdir(),"sawnwood.RData"))
}


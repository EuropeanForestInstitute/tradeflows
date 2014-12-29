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


#' Extract median prices at a given geographical aggregation level
#'
#' Extract median prices by region (default) or at another
#' geographical aggregation level (subregion)
#' @param dtf a dataframe containing already available prices
#' @param geoaggregation a character vector specifying the regional aggregation
#'     level, a column name in the reportercomtrade table
#' @param excludeqestimates logical TRUE when comtrade quantity estimates
#' have to be excluded
#' @export
extractprices <- function(dtf, geoaggregation="regionreporter",
                          excludeqestimates = FALSE){
    if(excludeqestimates){
        dtf <- dtf %>% filter(flag==0 |flag==4 )
    }
    dtf %>%
        filter(flow %in% c("Import", "Export")) %>%
        # Remove EU28 reporter
        filter(!reporter %in% c("EU-28")) %>%
        # Remove EU-28 and World partner
        filter(!partner %in%c("EU-28", "World")) %>%
        # Add regionreporter and regionpartner
        merge(select(reportercomtrade, reportercode, regionreporter=region)) %>%
#         merge(select(reportercomtrade,
#                      partnercode = reportercode, regionpartner=region)) %>%
        group_by(flow, regionreporter, year) %>%
        summarise(lowerprice = round(0.5 * quantile(price, 0.25,
                                                    names=FALSE, na.rm=TRUE)),
                  medianprice = round(median(price, na.rm=TRUE)),
                  upperprice = round(2 * quantile(price, 0.75,
                                                  names=FALSE, na.rm=TRUE))) %>%
        arrange(-medianprice)
}



#' Extract median converion factor at a given geographical aggregation level
#'
#' Extract median converion factors for the whole world (default)
#' @param dtf a dataframe containing conversion factors
#' @param geoaggregation a character string specifying the regional aggregation
#'     level, a column name in the reportercomtrade table
#' @param  excludeqwestimates logical TRUE when comtrade quantity and weight
#'          estimates have to be excluded
#' @export
extractconversionfactors <- function(dtf, geoaggregation="World",
                                     excludeqwestimates=FALSE){
    if(excludeqwestimates){
        dtf <- dtf %>% filter(flag==0)
    }
    dtf %>%
        filter(flow %in% c("Import", "Export")) %>%
        # Remove EU28 reporter
        filter(!reporter %in% c("EU-28")) %>%
        # Remove World partner
        filter(!partner %in%c("EU-28", "World")) %>%
        group_by(flow, year) %>%
        summarise(medianconversion = round(median(conversion,na.rm=TRUE)))
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
#'  tests for duplicated reportercode, partnercode, productcode, flow, year
#' @export
duplicates <- function(dtf){
    dtf$duplicate <- duplicated(select(dtf, reportercode, partnercode,
                                       productcode, flow, year))
    filter(dtf, duplicate)
}


#' Remove duplicates
#' Print the number of duplicated entries and remove them
removeduplicates <- function(dtf){
    if(nrow(duplicates(dtf))>1){
        message("There were duplicated lines for the following reporters:")
        message(unique(duplicates(dtf)$reporter))
        return(unique(dtf))
    }
    return(dtf)
}


#' Add volume and value of the partner flow
#'
#' For the same country, year, item, add the corresponding partner flow.
#'@param dtf data frame containing trade flow data.
#'   With column names following the efi convention.
#'@export
addpartnerflow <- function(dtf){
    # Warning for duplicated entries
    if(nbduplicates(dtf)>1){
        stop("Remove duplicated entries before adding partner flows")
    }
    swap <- dtf %>%
        rename(partnercode = reportercode,
               reportercode = partnercode) %>%
        select(reportercode, partnercode,
               productcode, flow, year,
               quantity, weight, tradevalue) %>%
        mutate(flow = gsub("Import","aaaaaaa",flow),
               flow = gsub("Export","Import",flow),
               flow = gsub("aaaaaaa","Export",flow))
    # Check that values in the dtf and swap tables are all there
    # and in the same order
    stopifnot(dtf$value == swap$value)
    # Merge
    dtf <- merge(dtf, swap, all.x=TRUE, suffixes = c("", "partner"),
                 by = c("reportercode", "partnercode",
                        "productcode", "flow", "year"))
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



#' Calculate volume based on tradevalue




#' Combine all cleaning functions
#' @param dtf data frame
#' @param write2db logical TRUE to write the result in the database
#' @export
clean <- function(dtf, write2db=FALSE){
    dtf %>%
        addpartnerflow() %>%
        calcunitprices()
}


if(FALSE){
    library(dplyr)
    load("data-raw/sawnwood_all.RData")
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


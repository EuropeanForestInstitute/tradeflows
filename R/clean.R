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
    dtf <- dplyr::select_(dtf, .dots = column_names$efi)
    return(dtf)
}


#' Remove duplicates
#' Print the number of duplicated entries and remove them
removeduplicates <- function(dtf){
    nbduplicates <- sum(duplicated(select(dtf, reportercode, partnercode,
                                          productcode, flow, year)))
    if(nbduplicates>1){
        message("There were duplicated lines, we removed them.")
        print(summary(duplicated(dtf)))
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
    nbduplicates <- sum(duplicated(select(dtf, reportercode, partnercode,
                                          productcode, flow, year)))
    if(nbduplicates>1){
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


#' Calculate unit prices
#'
#' Calculate unit prices in current currency
#' @param dtf data frame
#' @import dplyr
calcunitprices <- function(dtf){
    dtf %>% mutate(pricecur = tradevalue / weight )
}


#' Combine all cleaning functions
#' @param dtf data frame
#' @export
clean <- function(dtf){
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


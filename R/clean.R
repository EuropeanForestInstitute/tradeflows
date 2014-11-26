################################# #
# Functions that clean trade data #
################################# #

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

#' Calculate unit prices
#'
#' Calculate unit prices in current currency
#' @param dtf data frame
#' @import dplyr
calcunitprices <- function(dtf){
    dtf %>% mutate(pricecur = tradevalue / weight )
}


if(FALSE){
    library(dplyr)
    load("data-raw/sawnwood.RData")
    sawnwood <- sawnwood %>%
        renamecolumns %>%
        calcunitprices
    save(sawnwood, file="data/sawnwood.RData")
}


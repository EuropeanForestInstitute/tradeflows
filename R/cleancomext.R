

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

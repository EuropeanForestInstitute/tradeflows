# Tests written with the Comext data structure in mind
# no mirror flow (except for European Countries)

require(dplyr, warn.conflicts = FALSE, quietly = TRUE)

context("addconversionfactorandprice")
test_that("Add prices works for both prices per quantity and prices per weight",{
    dtf <- data_frame(tradevalue = c(12, 24, 36),
                      quantity = c(2, 4, 6),
                      weight = c(3, 6, 9)) %>%
        addconversionfactorandprice()
    expect_equal(dtf$price, c(6, 6, 6))
    expect_equal(dtf$pricew, c(4, 4, 4))
})


context("extractprices")
test_that("",{
    dtf <- data_frame(flowcode = c(1,1,1,2,2,2),
                      regionpartner = c("Europe","Europe","Asia",
                                        "Europe","Europe","Asia"),
                      year = 2017,
                      price = c(1,2,3,1,2,3),
                      tradevalue  = 1:6,
                      quantity = 1:6)

    # Default settings of the extractprices() function
    dtf_default <- dtf %>%
        extractprices(grouping = c("flowcode", "regionpartner", "year"))
    # extractprices() arranges the data frame
    # by decreasing order of median price
    expect_equal(dtf_default$lowerprice, rep(c(1.5, 0.625), each = 2))
    expect_equal(dtf_default$medianprice, rep(c(3, 1.5), each = 2))
    expect_equal(dtf_default$upperprice, rep(c(6,3.5), each = 2))

    # Comext settings of the extractprices() function
    dtf_comext <- dtf %>%
        extractprices(grouping = c("flowcode", "regionpartner", "year"),
                      lowercoef = 1, uppercoef = 1,
                      lowerquantile = 0.05, upperquantile = 0.95)

})


context("extractpricew")
test_that("",{
    dtf <- data_frame(flow = c(1,1,1,1,1,2,2,2,2,2),
                      regionpartner = c("Europe","Europe","Asia",
                                        "Europe","Europe",
                                        "Europe","Europe","Asia",
                                        "Asia", "Asia"),
                      year = 2017,
                      pricew = c(1,2,3,Inf,NA,1,2,3,Inf,NA),
                      tradevalue  = 1:10,
                      weight = 1:10) %>%
        extractpricew(grouping = c("flow", "regionpartner", "year"),
                      lowercoef = 1, uppercoef = 1)
    # extractpricew doesn't rearange the data frame
    # (in contrast to extractprices)
    # extractpricew now uses 5th percentile and 95th percentile as
    # lower and upper boundary on pricew.
    lower <- c(quantile(3, 0.05, names=FALSE),   # Asia a long statement to write 3
               quantile(1:2, 0.05, names=FALSE)) # Europe
    upper <- c(quantile(3, 0.95, names=FALSE),   # Asia a long statement to write 3
               quantile(1:2, 0.95, names=FALSE)) # Europe
    expect_equal(dtf$lowerpricew, rep(lower, 2))
    expect_equal(dtf$medianpricew, rep(c(3, 1.5), 2))
    expect_equal(dtf$upperpricew, rep(upper, 2))
})



context("joinpricecvfbounds")
test_that("The price and conversion factor bounds are added
          based on the gouping variables",{
    dtf <- data_frame(key = c("a", "b", "b", "a"),
                      year = c(2017, 2017, 2018, 2017),
                      unit = 1,
                      quantity = 1:4)
    price <- data_frame(key = c("a", "b", "b"),
                        year = c(2017, 2017, 2018),
                        unit = 1,
                        lowerprice = 1:3,
                        upperprice = 7:9)
    conversion <- data_frame(key = c("a", "b", "b"),
                             year = c(2017, 2017, 2018),
                             unit = 1,
                             lowerconversion = 11:13,
                             upperconversion = 17:19)
    dtf <- joinpricecvfbounds(dtf, price, conversion)
    # After the merge, dtf values will be
    # rearranged by the merge variables: key, year, unit
    expect_equal(dtf$lowerprice, c(1, 1, 2, 3))
    expect_equal(dtf$lowerconversion, c(11, 11, 12, 13))
})


context("estimatequantity")
test_that("the estimate quantity function merges prices and conversion factors
          with a simple key",{
    # estimatequantity is a simple merge with quantity and weight.
    # The merge doesn't speficy key variables, can this be an issue?
    dtf <- data_frame(period = 1:3,
                      flow = 1,
                      unit = "m3",
                      tradevalue = c(1,1,1),
                      quantity = c(1,NA,NA),
                      weight = c(1,1,NA),
                      flag = 0)
    price <- data_frame(period = 1:3,
                        flow = 1,
                        unit = "m3",
                        medianprice = c(10,20,30))
    conversionfactor <- data_frame(period = 1:3,
                                   flow = 1,
                                   unit = "m3",
                                   medianconversion = c(0.5,0.5,0.5))
    # by default, merge() merges by columns that are common in the 2 datasets
    intersect(names(dtf), names(price))
    intersect(names(dtf), names(conversionfactor))
    # merge is present in the current (as of September 2017) implementation of
    # the estimatequantity function
    dtf2 <- dtf %>%
        joinpricecvfbounds(price, conversionfactor) %>%
        estimatequantity()
    expect_equal(dtf2$quantity, c(1, 2, 1/30))
})


context("shaveprice")
test_that("",{
    dtf <- data_frame(key = 1:3,
                      tradevalue = c(2, 1, 12),
                      quantity = c(1, 2, 3),
                      quantity_up = c(4, 5, 6),
                      price = c(2, 0.5, 4),
                      lowerprice = c(1, 1, 1),
                      upperprice = c(3, 3, 3),
                      flag = 0,
                      flow = 1) %>%
        shaveprice()
    expect_equal(dtf$quantity, c(1,5,6))
})


context("shavepricew")
test_that("",{
    dtf <- data_frame(key = 1:3,
                      tradevalue = c(2, 4, 12),
                      weight = c(1, 8, 3),
                      pricew = c(2, 0.5, 4),
                      lowerpricew = c(1, 1, 1),
                      upperpricew = c(3, 3, 3),
                      flag = 0,
                      flow = 1) %>%
        shavepricew()
    # If pricew < lowerpricew, then weight = tradevalue / lowerpricew
    # If pricew > upperpricew, then weight = tradevalue / upperpricew
    expect_equal(dtf$weight, c(1, 4, 4))
})


context("shavecvf")
test_that("",{
    dtf<- data_frame(weight = c(2, 1, 4),
                     quantity = c(1, 2, 1),
                     conversion = c(2, 0.5, 4),
                     lowerconversion = c(1, 1, 1),
                     upperconversion = c(3, 3, 3),
                     flag = 0,
                     flow = 1) %>%
        shaveconversion()

    # If conversion < lowerconversion, then quantity = weight / lowerconversion
    # If conversion > upperconversion, then quantity = weight / upperconversion
    expect_equal(dtf$quantity, c(1, 1, 4/3))
})


context("changeflowmessage")
test_that("",{
    dtfbefore_UN <- data_frame(flow = c("Import","Export"),
                               quantity = c(1,2))
    dtfafter_UN <- data_frame(flow = c("Import","Export"),
                              quantity = c(2,3))
    dtfbefore_EU <- data_frame(flow = c(1,2),
                               quantity = c(1,2))
    dtfafter__EU <- data_frame(flow = c(1,2),
                               quantity = c(3,4))
    changeflowmessage(dtfbefore_UN, dtfafter_UN)
    changeflowmessage(dtfbefore_EU, dtfafter__EU)
})


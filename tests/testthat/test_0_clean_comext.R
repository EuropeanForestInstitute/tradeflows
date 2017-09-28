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
    dtf <- data_frame(flow = c(1,1,1,2,2,2),
                      regionpartner = c("Europe","Europe","Asia",
                                        "Europe","Europe","Asia"),
                      year = 2017,
                      price = c(1,2,3,1,2,3),
                      tradevalue  = 1:6,
                      quantity = 1:6) %>%
        extractprices(grouping = c("flow", "regionpartner", "year"))
    # extractprices() arranges the data frame
    # by decreasing order of median price
    expect_equal(dtf$lowerprice, rep(c(1.5, 0.625), each = 2))
    expect_equal(dtf$medianprice, rep(c(3, 1.5), each = 2))
    expect_equal(dtf$upperprice, rep(c(6,3.5), each = 2))
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
        extractpricew(grouping = c("flow", "regionpartner", "year"))
    # extractpricew doesn't rearange the data frame
    # (in contrast to extractprices)
    expect_equal(dtf$lowerpricew, rep(c(1.5, 0.625),2))
    expect_equal(dtf$medianpricew, rep(c(3, 1.5),2))
    expect_equal(dtf$upperpricew, rep(c(6,3.5),2))
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
    dtf2 <- estimatequantity(dtf, price, conversionfactor)
    expect_equal(dtf2$quantity,c(1, 2, 1/30))
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


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
    expect_equal(dtf$lowerprice, rep(c(1.5, 0.625),2))
    expect_equal(dtf$medianprice, rep(c(3, 1.5),2))
    expect_equal(dtf$upperprice, rep(c(6,3.5),2))
})


context("estimatequantity")
test_that("the estimate quantity function works with a simple key",{
    # estimatequantity is a simple merge with quantity and weight.
    # The merge doesn't speficy key variables, can this be an issue?
    dtf <- data_frame(key = 1:3,
                      tradevalue = c(1,1,1),
                      quantity = c(1,NA,NA),
                      weight = c(1,1,NA))
    price <- data_frame(key = 1:3,
                        price = c(10,20,30))
    conversionfactor <- data_frame(key = 1:3,
                                   c(0.5,0.5,0.5))
    # by default, merge() merges by columns that are common in the 2 datasets
    intersect(names(dtf), names(price))
    intersect(names(dtf), names(conversionfactor))
    # merge is present in the current (as of September 2017) implementation of
    # the estimatequantity function
    estimatequantity(dtf, price, conversionfactor)
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
                      flag = 0) %>%
        shaveprice()
})


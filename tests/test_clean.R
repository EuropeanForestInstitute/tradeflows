library(testthat)
library(tradeflows)

# This test suite assumes that there is a test dataset called sawnwoodexample.
swd <- tradeflows::sawnwoodexample

source("R/clean.R")


test_that("The total quantity before and after applying each cleaning function
          remains the same.", {
    # This test will fail for functions that correct the quantity.
    swd2 <- calcunitprices(swd)
    expect_that(sum(swd$quantity, na.rm=TRUE),
                equals(sum(swd2$quantity, na.rm=TRUE)))
    expect_that(sum(swd$tradevalue, na.rm=TRUE),
                equals(sum(swd2$tradevalue, na.rm=TRUE)))
    expect_that(sum(swd$weight, na.rm=TRUE),
                equals(sum(swd2$weight, na.rm=TRUE)))
})

require(dplyr, warn.conflicts = FALSE)

# This test suite assumes that
# The package contains a test dataset called sawnwoodexample.
#
# Note: unexported functions will be available to testthat without the need to load them.
# But to develop the tests.
# use devtools::load_all() or CTRL-SHIFT-L all to load unexported functions.
swd <- tradeflows::sawnwoodexample


context("Preparatory functions: addregion, addprice, etc")

test_that("Add region works",{
    dtf <- data_frame(reportercode = c(4,8,716),
                      partnercode = c(716,894,8)) %>%
        addregion()
    expect_that(dtf$regionreporter, equals(c("Africa", "Asia", "Europe")))
    expect_that(dtf$regionpartner, equals(c("Europe", "Africa", "Africa")))
})


test_that("Price and conversion factorcalculation, deals with NA", {
    dtf <- data.frame(weight = c(1,NA,5),
                      quantity = c(2,4,6),
                      tradevalue = c(3,NA,7))
    dtf2 <- dtf %>%
        addconversionfactorandprice()
    expect_that(dtf$tradevalue / dtf$quantity,
                    equals(dtf2$price))
    expect_that(dtf$weight / dtf$quantity,
                equals(dtf2$conversion))
})


test_that("Price extraction ", {
    dtf <- data_frame(reporter = letters[1:6],
                      partner = letters[21:26],
                      flow = rep(c("Import","Export"),3),
                      regionreporter = rep(c("a","a","b"),2),
                      tradevalue = rnorm(6,100000,10000),
                      quantity = rnorm(6,1000,100),
                      unit = "m3",
                      year = 2013) %>%
        mutate(price = tradevalue / quantity)
    dtf2 <- dtf %>% extractprices()
    # message("Try to use a different geoaggregation level in clean")
})


test_that("Grouping works in priceextraction", {
    dtf <- data.frame(price = rnorm(3,10),
                      aggregationregion = c("a","a","b"))
    # message("Use a different geoaggregation level in clean")
})


context("Partner flows")
options(tradeflows.verbose = FALSE)

# According to Hadley Wickham's book on R development and testing.
# Setup and teardown methods are not needed because of R's copy on modify principle
mockflows <- data_frame(productcode = c(440349, 440349, 440349, 440349),
                        flow = c("Import", "Export","Import", "Export"),
                        period = c(2010L, 2010L, 2011L, 2011L),
                        flag = c(0, 4, 0, 4),
                        reporter = c("China", "Malaysia","China", "Malaysia"),
                        reportercode = c(156, 458, 156, 458),
                        partner = c("Malaysia", "China", "Malaysia", "China"),
                        partnercode = c(458, 156, 458, 156),
                        tradevalue = c(84356413, 28229869, 95433402, 15376022),
                        quantity = c(391076, 185550, 278377, 89178),
                        price = c(NA, NA, NA, NA),
                        lowerprice = c(321, 229, 321, 229),
                        medianprice = c(465, 965, 465, 965),
                        upperprice = c(1139, 1939, 1139, 1939))

test_that("chosereporterorpartner keeps NA values in the standard deviation of prices",{
    # A standard deviation cannot be calculated with only one price sd() returns NA in this case.
    # This is not what I want to test. Therefore use more that 2 flows per direction
    dtf <- mockflows %>%
        # Add 4 more rows
        mutate(period = c(2008,2008,2009,2009)) %>%
        rbind(mockflows)
    # Introduce one NA
    dtf$quantity[1] <- NA
    dtf <- dtf %>%
        addpartnerflow %>%
        choosereporterorpartner()
    expect_that(dtf$favorpartner, equals(c(NA,NA)))
})


test_that("replacebypartnerquantity doesn't replace by a missing quantity",{
    # This is needed because the choice is often made over a shorter period
    # than the length of the actual data


})


test_that("Replacebypartnerquantity leaves us with one quantity for both mirror flows, even in the case of price shaving ", {
    # fake the creation of the choice data frame
    choice <- mockflows %>%
        slice(1:2) %>%
        select(flow, reportercode, partnercode,
                             reporter, partner) %>%
        mutate(favorpartner = c(TRUE,FALSE))
    # Clean the data partially
    dtf <- mockflows %>%
        addpartnerflow() %>%
        mutate(quantity_up = tradevalue / medianprice) %>%
        shaveprice()
    # dtf$quantity contains differing quantities for mirror flows
    dtf <- dtf %>% replacebypartnerquantity(choice)
    # dtf$quantity contains similar quantities for mirror flows
    dtf2010 <- dtf %>% filter(period ==2010)
    expect_that(dtf2010$quantity[1], equals(dtf2010$quantity[2]))
})


context("Clean functions do not change total quantity")
# Test that the total quantity before and after applying each
# cleaning function remains the same. Or that at least the number of
# rows stays the same
test_that("addregion() doesn't change total quantity", {
    swd2 <- addregion(swd)
    expect_that(sum(swd$quantity, na.rm=TRUE),
                equals(sum(swd2$quantity, na.rm=TRUE)))
    expect_that(sum(swd$tradevalue, na.rm=TRUE),
                equals(sum(swd2$tradevalue, na.rm=TRUE)))
    expect_that(sum(swd$weight, na.rm=TRUE),
                equals(sum(swd2$weight, na.rm=TRUE)))
})


test_that("addconversionfactorandprice() doesn't change total quantity",{
    swd2 <- addconversionfactorandprice(swd)
    expect_that(sum(swd$quantity, na.rm=TRUE),
                equals(sum(swd2$quantity, na.rm=TRUE)))
})


# message("Currently using sawnwoodexample, but
# estimatequantity() would require a full dataset with all
# world trade flows for one product to prepare conversion factors and prices")
# That would be an integration test
test_that("The total quantity remains approximately the same after
          estimatequantity().",{
#     swd2 <- swd %>% addregion %>% addconversionfactorandprice %>%
#               estimatequantity
#     stop("estimatequantity() would require a full dataset
#               to prepare conversion factors and prices")
          })


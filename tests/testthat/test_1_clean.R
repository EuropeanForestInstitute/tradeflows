require(dplyr, warn.conflicts = FALSE, quietly = TRUE)
options(tradeflows.verbose = FALSE)

# Note:
# When running this test suite with CTRL-SHIFT-T or devtools::test()
# unexported functions will be available without the need to load them.
# But to run this test suite on its own
# use devtools::load_all() or CTRL-SHIFT-L all to load unexported functions.

# According to Hadley Wickham's book on R development and testing.
# Setup and teardown methods are not needed because of R's copy on modify principle
# All objects created within test_that() calls are temporary.
# Objects are dropped at the end of the call. If an object was modified
# it was only the copy of that object that was modified.
# And the original is left in its original state
# a <- 1
# test_that("Modify variable in a test", {a <- 2})
# test_that("Variable stays unchanged for the next test",
#           {expect_that(a, equals(1))})

# This test suite assumes that
# The package contains a test dataset called sawnwoodexample.
swd <- tradeflows::sawnwoodexample


# Dummy trade flows ####
# This test suite also uses dummy trade flows data
dummytf <- data_frame(reporter = letters[1:6],
                      partner = letters[21:26],
                      flow = rep(c("Import","Export"),3),
                      regionreporter = rep(c("West","West","East"),2),
                      tradevalue = rlnorm(6,11,3),
                      quantity = rlnorm(6,7,3),
                      weight = rlnorm(6,11,3),
                      unit = "m3",
                      year = 2013,
                      lastchanged = 1481435872+1:6) %>%
    addconversionfactorandprice()

# Compare to real data
# Draw test data from a distribution similar to real data
if(FALSE){
    library(tradeflows)
    swd <- readdbproduct(440799, "raw_flow_yearly")
    hist(log(swd$weight))
    hist(log10(swd$weight))
    hist(log(rlnorm(5e5,meanlog = 11)))
    summary(swd[c("quantity","weight","tradevalue")])
    summary(rlnorm(1e6,15,2))
    summary(rlnorm(1e6,11,5))
    summary(rlnorm(1e6,11,3)) # weight and tradevalue
    summary(rlnorm(1e6,8,3))
    summary(rlnorm(1e6,7,3)) # quantity
}

# Prices and conversion factors ####
context("Prices and conversion factors")

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


test_that("Price extraction works with different aggregation levels ", {
    priceregional <- dummytf %>%
        extractprices()
    priceglobal <- dummytf %>%
        extractprices(grouping = c("flow", "year", "unit"))
})


test_that("Conversion factor extraction ignores Inf and O values ", {
    dummytf$conversion[1] <- Inf
    dummytf$conversion[3] <- 0
    median(dummytf$conversion)
    cf1 <- dummytf %>%
        extractconversionfactors()
    if(FALSE){ # to delete
        dummytf[c("flow", "regionreporter", "conversion")]
        dummytf %>%
            filter(!is.infinite(conversion) & !conversion ==0) %>%
            select(flow, regionreporter, conversion)
    }
    cf2 <- dummytf %>%
        filter(!is.infinite(conversion) & !conversion ==0) %>%
        group_by(flow, regionreporter, year, unit) %>%
        summarise(medianconversion = median(conversion,na.rm=TRUE))
    expect_equal(cf1$medianconversion, cf2$medianconversion)
})


test_that("Grouping works in priceextraction", {
    dtf <- data.frame(price = rnorm(3,10),
                      aggregationregion = c("a","a","b"))
    # message("Use a different geoaggregation level in clean")
})


context("Partner flows")
mockflows <- data_frame(productcode = c(440349, 440349, 440349, 440349),
                        flow = c("Import", "Export","Import", "Export"),
                        period = c(2010L, 2010L, 2011L, 2011L),
                        flag = c(0, 4, 0, 4),
                        reporter = c("China", "Malaysia","China", "Malaysia"),
                        reportercode = c(156, 458, 156, 458),
                        reporteriso = c("CHN", "MYS", "CHN","MYS"),
                        partner = c("Malaysia", "China", "Malaysia", "China"),
                        partnercode = c(458, 156, 458, 156),
                        partneriso = c("MYS", "CHN","MYS","CHN"),
                        tradevalue = c(84356413, 28229869, 95433402, 15376022),
                        quantity = c(391076, 185550, 278377, 89178),
                        price = c(NA, NA, NA, NA),
                        lowerprice = c(321, 229, 321, 229),
                        medianprice = c(465, 965, 465, 965),
                        upperprice = c(1139, 1939, 1139, 1939),
                        lastchanged = 1481435872 + 1:4)


test_that("chosereporterorpartner keeps NA values in the standard deviation of prices",{
    # A standard deviation cannot be calculated with only one price sd() returns NA in this case.
    # This is not what I want to test. Therefore use more that 2 flows per direction
    dtf <- mockflows %>%
        # Add 4 more rows
        mutate(period = c(2012,2012,2013,2013)) %>%
        rbind(mockflows)
    # Introduce one NA
    dtf$quantity[1] <- NA
    dtf <- dtf %>%
        addpartnerflow %>%
        choosereporterorpartner()
    expect_equal(dtf$favorpartner, c(NA,NA))
})


test_that("replacebypartnerquantity doesn't replace by a missing quantity",{
    # This test is needed and not redundant with the test above.
    # Because the choice is often made over a shorter period
    # than the length of the actual data favor partner can tell replacebypartnerquantity()
    # to replace by a quantity that is not available.
    dtf <-  mockflows %>% addpartnerflow()
    choice <- dtf %>% choosereporterorpartner()
    # Introduce an NA value in the partner quantity
    dtf$quantity[dtf$period == 2010 & dtf$reporter == "Malaysia"] <- NA
    dtf <- dtf %>% replacebypartnerquantity(choice)
    dtf$quantity[dtf$period == 2010 & dtf$reporter == "China"]
    expect_that(dtf$quantity[dtf$period == 2010 & dtf$reporter == "China"],
                equals(mockflows$quantity[mockflows$period == 2010 & mockflows$reporter == "China"]))
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


test_that("ISO code is correct after mirror flow copying", {
    # We should actually check that iso code matches countrycode and country name
    dtf <- mockflows %>%
        filter(reporter == "China") %>%
        addmissingmirrorflow()
    expect_that(dtf$reporteriso, equals(c("CHN","CHN","MYS","MYS")))
    expect_that(dtf$partneriso, equals(c("MYS","MYS","CHN","CHN")))
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


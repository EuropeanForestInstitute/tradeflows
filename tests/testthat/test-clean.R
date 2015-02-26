
# This test suite assumes that
# The package contains a test dataset called sawnwoodexample.
swd <- tradeflows::sawnwoodexample

context("Test that the total quantity before and after applying each
cleaning function remains the same. Or that at least the number of
rows stays the same")

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

message("Currently using sawnwoodexample, but
estimatequantity() would require a full dataset with all
world trade flows for one product to prepare conversion factors and prices")
test_that("The total quantity remains approximately the same after
          estimatequantity().",{
#     swd2 <- swd %>% addregion %>% addconversionfactorandprice %>%
#               estimatequantity
#     stop("estimatequantity() would require a full dataset
#               to prepare conversion factors and prices")
          })


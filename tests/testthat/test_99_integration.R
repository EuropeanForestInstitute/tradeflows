
# Database integration tests
context("Comext tables")

test_that("Data from raw comext table is the same as data from the text file", {
    # Monthly data
    comextf <- comexttextfile(productcode_ = 44013100, periodicity = "monthly")
    comextdb <- readdbproduct(tableread = "raw")
    expect_that(comextf, equals(comextdb))
})

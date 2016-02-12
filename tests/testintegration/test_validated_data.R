
# Database integration tests
context("Codes and corresponding elements are unique ")
message("These tests probably point to issues in the database structure.
        There are more than one key for each meaning for example flow and flowcode.")

testproduct <- 440799
# Load a sample of raw data
dtfr <- readdbproduct(440799, "raw_flow_yearly")
# Clean
dtfv <- dtfr %>% clean()
# Alternatively load validated data from the database directly


test_that("Test that there is a unique combination of flowcode and flow ", {
    # Remove this once the testthat package is installed
    stopifnot(lengthunique(dtfv, "flow", "flowcode") == lengthunique(dtfv, "flow"))
    stopifnot(lengthunique(dtfr, "flow", "flowcode") == lengthunique(dtfr, "flow"))
    # End remove this
    expect_that(lengthunique(dtfv, "flow", "flowcode"), equals(lengthunique(dtfv, "flow")))
    expect_that(lengthunique(dtfr, "flow", "flowcode"), equals(lengthunique(dtfr, "flow")))
})

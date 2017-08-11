# Tests below will only run if a test database is available
# RMySQL::mysqlHasDefault() checks if a default database is available
#
# If needed, create a test database
# MySQL should be installed on your system
# connect to the mysql client as root:
# $ mysql -u root -p
# Then give the following mysql commands:
# mysql> create database test;
# mysql> connect test;
# mysql> grant all privileges on * . * to R@localhost;
# Then configure the rs-dbi group by adding those lines to the ~/.my.cnf
#    [rs-dbi]
#    user = R
#    password = ***
#    host = localhost
#    database = test
#
# See RMySQL test examples at:
# https://github.com/rstats-db/RMySQL/tree/master/tests/testthat

# Create dummy codes
# Declare numeric values as integer data type
raw_code <- data.frame(code = c(4L, 4L, 4L),
                       datestart = c(1L, 2L, 2L),
                       description = c("a","b","b"),
                       stringsAsFactors = FALSE)
raw_product <- data.frame(productcode = c(44L, 44L),
                          datestart = c(1L, 2L))
raw_reporter <- data.frame(reportercode = c(5L, 6L, 5L),
                           datestart = c(1L, 2L, 2L))


context("Test database writable")
test_that("dummy data can be written to the database and read back", {
    # These tests will only run if a test database is available
    if (!RMySQL::mysqlHasDefault()) skip("Test database not available")
    # Connect to the database defined by the rs-dbi group
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    on.exit(RMySQL::dbDisconnect(con))
    # Write a csv file to disk
    write.csv(raw_code, "raw_code.csv", row.names = FALSE)
    # Transfer a csv file to the database
    RMySQL::dbWriteTable(con, "raw_code", "raw_code.csv", sep=",", eol="\n", overwrite = TRUE)
    expect_equal(RMySQL::dbReadTable(con, "raw_code"),
                 raw_code)
    # Transfer a data frame to the database
    RMySQL::dbWriteTable(con, "raw_code2", raw_code, row.names = FALSE,overwrite = TRUE)
    expect_equal(RMySQL::dbReadTable(con, "raw_code2"), raw_code)
})


context("cleancode")
test_that("codes correspond to the max(datestart) and are unique", {
    if (!RMySQL::mysqlHasDefault()) skip("Test database not available")
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    on.exit(RMySQL::dbDisconnect(con))
    # Write all codes to the database
    RMySQL::dbWriteTable(con, "raw_code", raw_code, row.names = FALSE, overwrite = TRUE)
    RMySQL::dbWriteTable(con, "raw_product", raw_product, row.names = FALSE, overwrite = TRUE)
    RMySQL::dbWriteTable(con, "raw_reporter", raw_reporter, row.names = FALSE, overwrite = TRUE)
    # Clean dummy codes
    # !Attention use unquoted name
    cleancode(con, tableread = "raw_code", tablewrite = "val_code",
              codevariable = code)
    val_code <- RMySQL::dbReadTable(con, "val_code", row.names=FALSE)
    # Test unique
    expect_equal(val_code$code, unique(raw_code$code))
    # Test most recent
    expect_equal(val_code$datestart, 2)
    # Clean product codes
    cleancode(con, tableread = "raw_product", tablewrite = "val_product",
              codevariable = productcode)
    val_product <- RMySQL::dbReadTable(con, "val_product")
    # Test unique
    expect_equal(val_product$code, unique(raw_product$code))
    # Test most recent
    expect_equal(val_product$datestart, 2)
    # Clean reporter codes
    cleancode(con, tableread = "raw_reporter", tablewrite = "val_reporter",
              codevariable = reportercode)
    val_reporter <- RMySQL::dbReadTable(con, "val_reporter")
    # Test unique
    expect_equal(val_reporter$code, unique(raw_reporter$code))
    # Test most recent
    expect_equal(val_reporter$datestart, c(2,2))
})


test_that("an error is raised if most recent codes are not exact duplicates", {
    if (!RMySQL::mysqlHasDefault()) skip("Test database not available")
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    on.exit(RMySQL::dbDisconnect(con))
    raw_code_deffect <- data.frame(code = c(4L, 4L, 4L),
                                   datestart = c(1L, 2L, 2L),
                                   description = c("a", "b", "c"),
                                   stringsAsFactors = FALSE)
    RMySQL::dbWriteTable(con, "raw_code", raw_code_deffect, row.names = FALSE, overwrite = TRUE)
    expect_error(cleancode(con, tableread = "raw_code", tablewrite = "val_code", codevariable = code),
                 regexp = "identical")
})


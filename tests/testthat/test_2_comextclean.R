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
raw_code <- data.frame(code = c(4L, 4L),
                       datestart = c(1L, 2L))
raw_product <- data.frame(productcode = c(44L, 44L),
                          datestart = c(1L, 2L))
raw_reporter <- data.frame(reportercode = c(5L, 6L),
                           datestart = c(1L, 2L))


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
    expect_equal(RMySQL::dbReadTable(con, "raw_code"), raw_code)
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
    cleancode(con, tableread = "raw_code", tablewrite = "val_code",
              codevariable = "code")
    val_code <- RMySQL::dbReadTable(con, "val_code")
    expect_equal(val_code, raw_code[max(raw_code$datestart),])
    expect_equal(val_code$code, unique(raw_code$code))
    # Clean product codes
    cleancode(con, tableread = "raw_product", tablewrite = "val_product",
              codevariable = "productcode")
    val_product <- RMySQL::dbReadTable(con, "val_product")
    expect_equal(val_product, raw_code[max(raw_product$datestart),])
    expect_equal(val_product$code, unique(raw_product$code))
    # Clean reporter codes
    cleancode(con, tableread = "raw_reporter", tablewrite = "val_reporter",
              codevariable = "productcode")
    val_reporter <- RMySQL::dbReadTable(con, "val_reporter")
    expect_equal(val_reporter, raw_code[max(raw_reporter$datestart),])
    expect_equal(val_reporter$code, unique(raw_reporter$code))
})



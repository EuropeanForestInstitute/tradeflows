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
raw_dummy_code <- data.frame(code = c(4L, 4L, 4L),
                       datestart = c(2001, 2002, 2002),
                       dateend = c(2001,2500,2500),
                       description = c("old","recent","recent"),
                       stringsAsFactors = FALSE)
raw_dummy_product <- data.frame(productcode = c(44L, 44L),
                                productdescription = c("old","recent"),
                                datestart = c("2001-01-01", "2002-01-01"),
                                dateend = c("2001-12-31", "2500-01-01"),
                                stringsAsFactors = FALSE)
raw_dummy_reporter <- data.frame(reportercode = c(5L, 6L, 5L),
                                 datestart = c(1L, 2L, 2L),
                                 reporter = c("old","recent","recent"),
                                 stringsAsFactors = FALSE)
# Create dummy database structure
createdbstructure("dummytables.sql", dbname = "test", sqlfolder = ".",verbose=FALSE)


context("Test database writable")
test_that("dummy data can be written to the database and read back", {
    # These tests will only run if a test database is available
    if (!RMySQL::mysqlHasDefault()) skip("Test database not available")
    # Connect to the database defined by the rs-dbi group
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    on.exit(RMySQL::dbDisconnect(con))
    # Write a csv file to disk
    write.csv(raw_dummy_code, "raw_dummy_code.csv", row.names = FALSE)
    # Transfer a csv file to the database
    RMySQL::dbWriteTable(con, "raw_dummy_code", "raw_dummy_code.csv", sep=",", eol="\n", overwrite = TRUE)
    expect_equal(RMySQL::dbReadTable(con, "raw_dummy_code"),
                 raw_dummy_code)
    # Transfer a data frame to the database
    RMySQL::dbWriteTable(con, "raw_dummy_code2", raw_dummy_code, row.names = FALSE,overwrite = TRUE)
    expect_equal(RMySQL::dbReadTable(con, "raw_dummy_code2"), raw_dummy_code)
})


context("cleancode")
test_that("codes correspond to the max(datestart) and are unique", {
    if (!RMySQL::mysqlHasDefault()) skip("Test database not available")
    createdbstructure("dummytables.sql", dbname = "test", sqlfolder = ".",verbose=FALSE)
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    on.exit(RMySQL::dbDisconnect(con))
    # Write all codes to the database
    RMySQL::dbWriteTable(con, "raw_dummy_code", raw_dummy_code, row.names = FALSE, append = TRUE)
    RMySQL::dbWriteTable(con, "raw_dummy_product", raw_dummy_product, row.names = FALSE, append = TRUE)
    RMySQL::dbWriteTable(con, "raw_dummy_reporter", raw_dummy_reporter, row.names = FALSE, append = TRUE)

    # Clean dummy codes
    # !Attention use unquoted name
    cleancode(con, tableread = "raw_dummy_code", tablewrite = "val_dummy_code",
              codevariable = code)
    # Clean product codes
    cleancode(con, tableread = "raw_dummy_product", tablewrite = "val_dummy_product",
              codevariable = productcode)
    # Clean reporter codes
    cleancode(con, tableread = "raw_dummy_reporter", tablewrite = "val_dummy_reporter",
              codevariable = reportercode)
    # Read back data
    val_dummy_code <- RMySQL::dbReadTable(con, "val_dummy_code", row.names=FALSE)
    val_dummy_product <- RMySQL::dbReadTable(con, "val_dummy_product")
    val_dummy_reporter <- RMySQL::dbReadTable(con, "val_dummy_reporter")
    # Test unique
    expect_equal(val_dummy_code$code, unique(raw_dummy_code$code))
    expect_equal(val_dummy_product$code, unique(raw_dummy_product$code))
    expect_equal(val_dummy_reporter$code, unique(raw_dummy_reporter$code))
    # Test most recent description
    expect_equal(val_dummy_code$description, "recent")
    expect_equal(val_dummy_product$productdescription, "recent")
    expect_equal(val_dummy_reporter$reporter, "recent")
})


test_that("an error is raised if most recent codes are not exact duplicates", {
    skip("want to see the mysql database content after the other test")
    if (!RMySQL::mysqlHasDefault()) skip("Test database not available")
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    on.exit(RMySQL::dbDisconnect(con))
    createdbstructure("dummytables.sql", dbname = "test", sqlfolder = ".",verbose=FALSE)
    raw_dummy_code_deffect <- data.frame(code = c(4L, 4L, 4L),
                                   datestart = c(1L, 2L, 2L),
                                   description = c("a", "b", "c"),
                                   stringsAsFactors = FALSE)
    RMySQL::dbWriteTable(con, "raw_dummy_code", raw_dummy_code_deffect, row.names = FALSE, overwrite = TRUE)
    expect_error(cleancode(con, tableread = "raw_dummy_code", tablewrite = "val_dummy_code", codevariable = code),
                 regexp = "identical")
})


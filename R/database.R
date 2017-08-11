#' Creates a database structure based on a sql dump file
#' @description Load a .sql file containing a database structure.
#' @details The default .sql file is storred in the package configuration folder.
#' @param sqlfile character name of an SQL file
#' @param dbname character name of a database
#' @param sqlfolder character path to the folder where the sqlfile is located
#' (defaults to the package config directory)
#' @param messageonly boolean if TRUE prints only a message and doesn't
#' transfer the database structure
#' @examples
#' # Load new database structures in the test database
#' # Load a database structure designed to contain raw data
#' createdbstructure(sqlfile = "raw_comext.sql", dbname = "test")
#' # Load a database structure designed to contain validated data
#' createdbstructure(sqlfile = "val_comext.sql", dbname = "test")
#' @export
createdbstructure <- function(sqlfile,
                              dbname = "tradeflows",
                              sqlfolder = system.file("config", package="tradeflows", mustWork = TRUE),
                              messageonly = FALSE){
    sqlfile <- file.path(sqlfolder, sqlfile)
    mysqlconfigmessage <-
    "In case of access denied try to edit the mysql configuration file in ~/.my.cnf, it should contain
    [client]
    user = usename
    password = password"
    if (messageonly){ # Only print a message
        message("This message gives 2 options to load the database structure.\n\n",
                "(1) If a user called 'R' is created in MySQL, you can run this from a shell command line: \n",
                sprintf("$ cat '%s' | mysql -u R -p %s", sqlfile, dbname),
                "\n\n",
                "(2) Another option is to call this from a mysql client:\n",
                sprintf("mysql> connect %s;\n", dbname),
                sprintf("mysql> source %s;", sqlfile))
        return()
    } else { # Load sqlfile into the database
        tryCatch({
            message("Loading table definitions from:\n", sqlfile,
                    "\ninto the `", dbname, "` database.")
            system(sprintf("cat '%s' | mysql %s", sqlfile, dbname), intern = TRUE)
            # Display the names of created tables
            createtables <- gsub("\\(","",grep("CREATE TABLE",readLines(sqlfile),value=TRUE))
            message(paste(createtables, collapse = "\n"))
        # In case of error or warning, print additional message
        }, error = function(errorcondition){
            message(toString(errorcondition), "\n", mysqlconfigmessage)
            stop(errorcondition)
        }, warning = function(warningcondition){
            message(toString(warningcondition), "\n", mysqlconfigmessage)
        }
        )
    }
}


#' Sets database connection parameters as a global R option tradeflowsDB
#'
#' Database connection parameters
#' Are storred in the installed version of the package
#' Run this command to find the location of the configuration file on your machine:
#' system.file("config/databaseconfig.R", package="tradeflows").
#' Use reload=TRUE to force reloading the file after modification.
#' @param reload logical, force reloading the configuration file
#' @param silent logical, do not print "already loaded" message when TRUE
#' @export
setdatabaseconfig <- function(reload = FALSE, silent = FALSE){
    if(is.null(getOption("tradeflowsDB"))|reload){
        # Path to the databasesonfig.R file
        databaseconfig <- system.file("config/databaseconfig.R",
                                      package="tradeflows")
        message(paste("Loading database configuration from ",
                      databaseconfig))
        source(databaseconfig)
    } else {
        if (!silent){
            message("Database configuration file already loaded.")
            message("Use the option reload=TRUE if you want to reload it.")
        }
    }
}


#' Control the presence of required columns in the
#' given database tables
#'
#' Database connection parameters are given by the global option
#' getOption("tradeflowsDB").
#' @param tables character vector containing the name of
#' database tables to check
#' @export
checkdbcolumns <- function(tables = c("raw_flow_yearly", "validated_flow_yearly")){
    require(dplyr)
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBread <- RMySQL::dbConnect(RMySQL::MySQL(), user=db["user"], host=db["host"],
                         password=db["password"], dbname=db["dbname"])
    for(tableread in tables){
        print(str(tableread))
        if(!tableread %in% names(column_names)){
            stop("Add ", tableread ," to the column_names table")
        }
        sqlquery <- paste(c("SELECT `COLUMN_NAME`",
                            "FROM `INFORMATION_SCHEMA`.`COLUMNS` ",
                            "WHERE `TABLE_SCHEMA`='tradeflows' ",
                            "AND `TABLE_NAME`='",tableread,"';"), collapse = "")
        res <- RMySQL::dbSendQuery(DBread, sqlquery)
        columnname <- RMySQL::dbFetch(res)
        columnname <- columnname$COLUMN_NAME
        # For that table, find the efi column names which are supposed to
        # be in there, but which are not in the data frame
        missingcolumns <- column_names$efi[column_names[c(tableread)] &
                                               !column_names$efi %in% columnname]
        if (length(missingcolumns)>0){
            warning("Following column_names are missing from the ",
                    tableread," table: \n",
                    paste(missingcolumns, collapse=", "))
        }
    }
    RMySQL::dbDisconnect(DBread)
}


#' Load trade flows for one product from a database table of choice
#'
#' Connect to the raw database and load all tradeflows
#' for the given product code,
#' for all years, in all directions, for all reporter and all partners.
#' @param productcode_ the code of a product
#' @param tableread the database table to read from
#' @param convcontrynames logical weather to convert contry names this should not be needed
#' if the database server is rendering utf-8 data.
#' @examples
#'\dontrun{
#' othersawnwod <- readdbproduct(440799, "raw_flow_yearly")
#' head(othersawnwod[c("year","reporter","partner","flow","tradevalue","quantity","weight")])
#' }
#' @export
readdbproduct <- function(productcode_, tableread, convcountrynames = FALSE){
    require(dplyr)
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    rawdata <- tbl(DBread, tableread)
    dtf <- rawdata %>%
        filter(productcode == productcode_) %>%
        # keep id in order to compare raw and cleaned trade flows
        # forces computation and brings data back into a data.frame
        collect()  %>%
        # Change year to an integer
        mutate(year = as.integer(year))
    if (convcountrynames){ # This should not be needed if the database is in utf-8
        dtf <- dtf %>% # Convert country names to utf-8
                mutate(reporter = iconv(reporter, "latin1", "utf-8"),
                   partner = iconv(partner, "latin1", "utf-8"))
        message("Changed reporter and partner encoding from latin1 to utf8.",
                "(check reportercode or partnercode 384 côte d'ivoire)")
    }
    # Comment out this check which might break for unnecessary reasons
    # if EFI developers decide to add extra columns in the database
    # stopifnot(names(dtf) %in% column_names$efi)
    #
    # When too many connections are opened, the following
    # error is returned:
    # Cannot allocate a new connection: 16 connections already opened
    # Remove connection object, base on Hadley's comment:
    # https://stackoverflow.com/questions/26331201/disconnecting-src-tbls-connection-in-dplyr/26331440#26331440
    rm(DBread)
    return(dtf)
}


#' Write validated flows for one product into the database
#'
#' Write data into the DB
#' @param dtf a data frame containing the data to be inserted
#' @param tablewrite character vector of a database table to write to
#' @export
writedbproduct <- function(dtf, tablewrite){
    # Write only to a validated_flow table
    stopifnot(tablewrite %in% c("validated_flow_yearly", "validated_flow_monthly"))
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBwrite <- RMySQL::dbConnect(RMySQL::MySQL(),
                                 user=db["user"], host=db["host"],
                                 password=db["password"], dbname=db["dbname"])
    dtf <- data.frame(dtf)
    result <- RMySQL::dbWriteTable(DBwrite, name = tablewrite,
                                   value=dtf, append=TRUE, row.names = FALSE)
    RMySQL::dbDisconnect(DBwrite)
    return(result)
}


#' Delete validated data for one product
#'@param product name
#'@export
deletedbproduct <- function(productcode, tabledelete){
    # Delete only in a validated_flow table
    stopifnot(tabledelete %in% c("validated_flow_yearly", "validated_flow_monthly"))
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBwrite <- RMySQL::dbConnect(RMySQL::MySQL(),
                                 user=db["user"], host=db["host"],
                                 password=db["password"], dbname=db["dbname"])
    sqlproduct <- paste("DELETE FROM ",tabledelete,
                         "WHERE productcode = ", productcode)
    res <- RMySQL::dbSendQuery(DBwrite, sqlproduct)
    nbrowsdeleted <- RMySQL::dbGetRowsAffected(res)
    message(nbrowsdeleted," rows were deleted.")
    RMySQL::dbClearResult(res)
    RMySQL::dbDisconnect(DBwrite)
    return(nbrowsdeleted)
}


#' Number of rows in a database table
#'
#' @param table name of a table
#' @export
nrowinDB <- function(tableread){
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    tabledata <- tbl(DBread, tableread)
    tabledata %>% summarise(nrow = n()) %>% collect
}


#' [Deprecated] Create a dplyr connector to MySQL
#' @description
#' \code{readdbtbl} is deprecated, use \code{dplyr::\link{tbl}} instead.
#' See example below.
#' @details [Deprecated] Old description
#'
#' Return a dplyr tbl object for the given MySQL database table
#' tbl objects allow lazy operations. See src_mysql.
#' The issue is that 2 table objects created with this function will
#' not share the same source.
#' Therefore they will not be available for a merge within the database.
#' @param tableread name of the database table to read
#' @return a dplyr tbl object
#' @examples
#'\dontrun{
#' # From 2017 onwards, use
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "tradeflows")
#' prod <- tbl(con, "raw_comext_cn")
#' RMySQL::dbDisconnect(con)
#' # Deprecated, before 2017
#' readdbtbl("raw_flow_yearly") %>%
#'     group_by(productcode) %>%
#'     summarise(nrow = n()) %>%
#'     arrange(nrow) %>%
#'     kable
#' }
#' @export
readdbtbl <- function(tableread){
    .Deprecated(new = "tbl", package = "dplyr",
                msg = "'readdbtbl' is deprecated. Use 'tbl' instead.
see example use in help('readdbtbl')
and a list of deprecated functions in help('tradeflows-deprecated')")
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    return(tbl(DBread, tableread))
}


#' Select rows by id
#'
#' @param table name of a table
#' @param id a vector of id
#' @export
selectbyid <- function(tableread, id){
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    tabledata <- tbl(DBread, tableread)
    tabledata %>% summarise(nrow = n()) %>% collect
}


if (FALSE){
    #fibre <- loadrawdata(550410)
    #     charcoal90 <- readdb(440290)
    swd99 <- readdb(440799, tableread = "raw_flow_yearly" )
    # See clean.R script for write example

    # Character set used in the database
    # found on https://github.com/rstats-db/RMySQL/issues/2
    setdatabaseconfig()
    db <- getOption("tradeflowsDB")
    library(RMySQL) # Just here, make full RMySQL calls in the functions
    DBwrite <- RMySQL::dbConnect(RMySQL::MySQL(),
                                 user=db["user"], host=db["host"],
                                 password=db["password"], dbname=db["dbname"])
    res <- RMySQL::dbSendQuery(DBwrite,"show variables like 'character_set_%'")
    dbFetch(res)
    dbGetQuery(DBwrite,"set names utf8")
    res <- dbSendQuery(DBwrite,"show variables like 'character_set_%'")
    dbFetch(res)
    RMySQL::dbDisconnect(DBwrite)

    # Can the same be done with a dplyr connection?
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])
    tbl(DBread, sql("show variables like 'character_set_%'")) %>%
        collect()
}


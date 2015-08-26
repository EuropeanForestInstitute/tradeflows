# This script defines report creation functions
# Each report has specific data requirements,
# Therefore functions fullfillthe data requirements of each report.
#
# createreport() and createreportfromdb() are generic function with no predefined template.
# createcompletenessreport(), creatediscrepancyreport() and createoverviewreport()
# are specific functions with a default template.
# The table below illustrates the number countries the number of products to be expected
# in each report type.
# |Report name  |Number of countries | Number of products | function                 |
# |------------:|:-------------------|--------------------|--------------------------|
# |overview     | one                | many               | createoverviewreport      |
# |completeness | many               | one                | createcompletenessreport |
# |discrepancy  | one                | one                | creatediscrepancyreport  |


#' Extract report metadata from trade flow table
#'
#' This can be used to check that the given trade flow data has only
#' one reporter and product or the empty string as expected by some reports.
#' @param tfdata a table of trade flows data
#' @return a list of metadata containing the productcode, reporter and unit elements
#' @export
extractmetadata <- function(tfdata){
    # Extract product code
    productcodeindata <- unique(tfdata$productcode)
    # In cases where there are many products replace this variable by ""
    if(length(productcodeindata)>1) productcodeindata <- ""

    #  Extract reporter
    nbyears <- length(unique(tfdata$year))
    reporterindata <- tfdata %>% group_by(reporter) %>%
        summarise(nrowperyear = n()/nbyears ) %>%
        # The one which has far more than one row per year
        filter(nrowperyear > 4)
    reporterindata <- reporterindata$reporter
    # In cases where there are many countries replace this variable by ""
    if(length(reporterindata)>1) reporterindata <- ""

    # Extract quantity unit
    unitindata <- tfdata %>% group_by(unit) %>%
        summarise(nrowperyear = n()/nbyears) %>%
        # Find the unit appearing most in the data
        filter(nrowperyear > 0.2 * max(nrowperyear))
    unitindata <- unitindata$unit

    return(list(productcode = productcodeindata,
                reporter = reporterindata,
                unit = unitindata))
}


#' Create data completeness report for comtrade data
#'
#' This function will generate reports for any template that is
#' product specific.  Giving information about all world trade
#' flows for one product.
#' @param tfdata a dataframe containing trade flows data that
#' will be passed to R code run by the template
#' @param inputpath path of the template, defaults to package internal path
#' @param template name of the template file, including .Rmd extension
#' @param fileprefix character string at the begining of the generated pdf file name
#' @param productcodeinreport numeric or character string leave this variable empty if there are many products
#' @param reporterinreport character string leave this variable empty if there are many reporters
#' @param outputdir name of the output directory relative to getwd()
#' @param encoding, encoding of the template file. See also iconv
#' The names of encodings and which ones are available are
#' platform-dependent. All R platforms support ""
#' (for the encoding of the current locale), "latin1" and "UTF-8".
#' @examples
#'\dontrun{
#'createreport(NULL, template="allproducts.Rmd", reporterinreport ="China")
#' }
#' @export
createreport <- function(tfdata,
                         template,
                         fileprefix = NULL,
                         productcodeinreport = NULL,
                         reporterinreport = NULL,
                         inputpath = system.file("templates",
                                                 package="tradeflows"),
                         outputdir = "reports",
                         encoding = "UTF-8",
                         toc = TRUE,
                         keep_tex = FALSE){
    # load optional packages
    require(ggplot2)
    require(reshape2)
    require(knitr)
    #     # Legacy check for colmuns, might be placed in the template
    #     if (!"productcode" %in% names(tfdata)){
    #         stop("Rename raw data columns to EFI convention before running this function.")
    #     }

    # Don't show R code in output
    knitr::opts_chunk$set(echo=FALSE, warning=FALSE)

    # If the path is relative, make it an absolute path
    # So that rmarkdown render works properly
    if(substr(outputdir, 1,1) != "/"){
        outputdir <- file.path(getwd(), outputdir)
    }
    # legacy check for a unitque product, report should be also generated for many products
    # if (length(unique(tfdata$productcode)) != 1){
    #         message("There should be only one product in the trade flows data frame")
    #         return(FALSE)
    #     }
    # Create the report file name
    filename <- paste0(fileprefix, productcodeinreport, reporterinreport, ".pdf")
    tryCatch(rmarkdown::render(input = file.path(inputpath, template),
                               output_format = rmarkdown::pdf_document(keep_tex = keep_tex,
                                                                       toc = toc),
                               output_dir = outputdir,
                               output_file = filename,
                               encoding = encoding),
             finally = print("Finally"))
}


#' Create report from the database
#'
#' For a given vector of product codes, create reports using a given template.
#' @description Create a report from the database
#' @rdname createrproducteport
#' @param template name of the template file
#' @param products a vector of product codes
#' @param ... arguments passed to \code{\link{createreport}()}
#'     from \code{createcompletenessreport}()
#' @export
createreportfromdb <- function(tableread,
                               products = "all",
                               ...){
    if (products=="all"){
        products <- productsindb(table = tableread)
    }
    # Loop on products
    for (productcodeinreport in products){
        message("creating report for product: ", productcodeinreport)
        dtf <- readdbproduct(productcodeinreport, tableread)
        createreport(tfdata = dtf, ...)
    }
}


#' Create reports for all products in the given reporter country
#'
#' The location of the report can be changed by changing the outputdir parameter.
#' See arguments of \code{\link{createreport}()}
#' to determine in which folder the template is located.
#' @param reporter_
#' @param template name of the template file.
#' @param outputdir path where report will be saved, relative to the working directory
#' or an absolute path.
#' @param ... further arguments passed to \code{\link{createreport}()}
#' @examples
#'\dontrun{
#' createoverviewreport("China", outputdir = "/tmp")
#' }
#' @export
createoverviewreport <- function(reporter_,
                                 beginyear = 0, endyear = 9999,
                                 template = "overview.Rmd",
                                 outputdir = "reports/overview", ...){

    message("Trade values are the same in raw flow and validated flow")
    message("The table read will have to be changed to validated_flow to access quantities")

    # If I use the readdbtbl function to join the 2 tables, the following error will be returned
    # Error: x and y don't share the same src. Set copy = TRUE to copy y into x's source (this may be time consuming).
    # Therefore I create the connection object here so that it can be shared between the two tbl() objects.
    setdatabaseconfig(silent=TRUE)
    db <- getOption("tradeflowsDB")
    DBread <- src_mysql(user=db["user"], host=db["host"],
                        password=db["password"], dbname=db["dbname"])

    # Load trade flow data --------------------------------------------------------
    tfdata <- tbl(DBread, "raw_flow_yearly") %>%
        filter(reporter == reporter_&
                   year >= beginyear & year <= endyear) %>%
        select(year, reporter, partner, partnercode,
               flow, flag, productcode, tradevalue, quantity)

    # Load itto product names --------------------------------------------------------
    message("Discuss with Simo and Janne to change column names in the product work table, rename them as such: (product = name_short, productcode = code) ")
    productitto <- tbl(DBread, "product_work") %>%
        filter(nomenclature == "HS12") %>%
        select(product = name_short, productcode = code)

    # Join tfdata and itto product names --------------------------------------------------
    # both are dplyr::tbl objects the statements will be converted to SQL
    tfdata <- tfdata %>%
        left_join(productitto) %>%  # joining late in the pipe, after filter is faster
        collect()

    # Report
    createreport(tfdata,
                 template = template,
                 outputdir = outputdir,
                 reporterinreport = reporter_, ...)
}


#' Create the completeness report
#'
#' @param productcode_ vector of product codes
#' @param ... arguments passed to \code{\link{createreport}()}
#' @examples
#'\dontrun{
#' createcompletenessreport(440799, beginyear = 2010, endyear = 2011)
#' }
#' @export
createcompletenessreport <- function(productcode_,
                                     beginyear = 0, endyear = 9999,
                                     template =  "completeness.Rmd",
                                     outputdir = "reports/completeness",
                                     toc = TRUE, ...){
    #### Load data ####
    rawtbl <- readdbtbl("raw_flow_yearly") %>%
        # dplyr verbs executed on tbl objects are translated to SQL statements
        # have to use this underscore trick because of the non standard evaluation
        # see vignette("nse") for more information on this
        filter(productcode == productcode_ &
                   year >= beginyear & year <= endyear)
    dtf <- rawtbl %>% collect %>%
        # Mysql datatype year is loaded as a character string
        # convert the year column to an integer
        mutate(year = as.integer(year))
    # delete mysqlconnection object, to avoid message of the type:
    # "Auto-disconnecting mysql connection (0, 3)"
    rm(rawtbl)

    #### Create the report ####
    createreport(tfdata = dtf,
                 template = template,
                 productcode = productcode_,
                 outputdir = outputdir,
                 toc = toc,
                 ...)
}


#' Create a discrepancy report
#'
#' @param productcode vector of product codes
#' @param reporter_ one single country name
#' @param ... arguments passed to \code{\link{createreport}()}
#' @examples
#'\dontrun{
#' creatediscrepancyreport(440799, "Cameroon", outputdir = "reports/discrepancies")
#' creatediscrepancyreport(440710, "Thailand", beginyear = 2010, endyear = 2012)
#' }
#' @export
creatediscrepancyreport <- function(productcode_, reporter_,
                                    beginyear = 0, endyear = 9999,
                                    template =  "discrepancy.Rmd",
                                    outputdir = "reports/discrepancies",
                                    toc = FALSE, ...){
    dtf <- readdbtbl("raw_flow_yearly") %>%
        filter(productcode == productcode_ &
                   year >= beginyear & year <= endyear &
                   (reporter == reporter_ | partner == reporter_)) %>%
        collect
    createreport(tfdata = dtf,
                 template = template,
                 productcode = productcode_,
                 reporter = reporter_,
                 outputdir = outputdir,
                 toc = toc,
                 ...)
}


if (FALSE){
    library(tradeflows)
    ###################### #
    # Overview reports     #
    ###################### #
    # Use the default template built within the package
    createoverviewreport(reporter_ = "Italy")
    # Use the template in development for quick itteration without package building


    ###################### #
    # Completeness reports #
    ###################### #
    # Use the default template built within the package
    # You need to rebuild the package for template updates to take effect
    createcompletenessreport(productcode_ = 440799)
    createcompletenessreport(productcode_ = 440710)
    # Use the template in development before package is build and select some years
    createcompletenessreport(440799, beginyear = 2010, endyear = 2011, inputpath = "inst/templates")
    # Use the createreportfromdb function with the template that will be exported with the package
    createreportfromdb("raw_flow_yearly", 440799, template = "completeness.Rmd",
                       outputdir = "reports/completeness/")

#     pandoc: Cannot decode byte '\xfc': Data.Text.Encoding.Fusion.streamUtf8: Invalid UTF-8 stream
#     Error: pandoc document conversion failed with error 1

    createcompletenessreport(tradeflows::sawnwoodexample, outputdir = directory)
    # report for the "black hole" dataset
    load("data-raw/comtrade/440799.RData")
    swd99 <- renamecolumns(dtf, "comtrade", "efi")
    createcompletenessreport(swd99, outputdir = directory)
    # another dataset
    load("data-raw/sawnwood_all.RData")
    swdall <- renamecolumns(swdall)
    createcompletenessreport(swdall, "4407", outputdir = directory)
    # Complete dataset from server
    load("data-raw/4409.RData")
    dtf <- renamecolumns(dtf)
    createcompletenessreport(dtf, outputdir = directory)


    ####################### #
    # Discrepancies reports #
    ####################### #
    creatediscrepancyreport(productcode_ = 440799, reporter_ = "Germany")
    # Old way using only the createreport() functio
    load("data-raw/comtrade/440799.RData")
    swd99 <- renamecolumns(dtf, "comtrade", "efi")
    createreport(swd99, outputdir = directory, template = "discrepancies.Rmd")


    ############################### #
    # Network visualisation reports #
    ############################### #
    directory <- "docs/development/networkvisualisation/"
    load("data-raw/comtrade/440799.RData")
    swd99 <- renamecolumns(dtf, "comtrade", "efi")
    createreport(swd99, outputdir = directory, template="asdfdsa.Rmd")
}

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
    unitinreport <- tfdata %>% group_by(unit) %>%
        summarise(nrowperyear = n()/nbyears) %>%
        # Find the unit appearing most in the data
        filter(nrowperyear > 0.2 * max(nrowperyear))
    unitinreport <- unitinreport$unit

    return(list(productcode = productcodeindata,
                reporter = reporterindata,
                unit = unitinreport))
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

#' Create reports for all products in the given country
#'
#' The location of the report can be changed by changing the outputdir parameter.
#' See arguments of \code{\link{createreport}()}
#' to determine in which folder the template is located.
#' @param country
#' @param template name of the template file.
#' @param outputdir path where report will be saved, relative to the working directory
#' or an absolute path.
#' @param ... further arguments passed to \code{\link{createreport}()}
#' @examples
#'\dontrun{
#' createcountryreport("China", outputdir = "/tmp")
#' }
#' @export
createcountryreport <- function(country, template = "allproducts.Rmd",
                                outputdir = "reports/countries", ...){
    # Convert country names could be a DB option in setdatabaseconfig
    createreport(NULL, template=template, outputdir = outputdir,
                 reporterinreport = country, ...)
}


#' Create a discrepancy report
#'
#' @param productcode vector of product codes
#' @param reporter one single country name
#' @param ... arguments passed to \code{\link{createreport}()}
#' @examples
#'\dontrun{
#' creatediscrepancyreport(440799, "Cameroon", outputdir = "reports/discrepancies")
#' }
#' @export
creatediscrepancyreport <- function(productcode_, reporter_,
                                    template =  "discrepancies.Rmd", ...){
    dtf <- readdbtbl("raw_flow_yearly") %>%
        filter(productcode == productcode_ &
                   (reporter == reporter_ | partner == reporter_)) %>%
        collect
    createreport(tfdata = dtf, template = template,
                        productcode = productcode_, reporter = reporter_, ...)
}

if (FALSE){
    # You need to rebuild the package for template updates to take effect
    library(tradeflows)

    ###################### #
    # Completeness reports #
    ###################### #
    directory <- "docs/development/completeness/"
    # Template that will be exported with the package
    createreportfromdb("raw_flow_yearly", 440799, template = "completeness.Rmd",
                       outputdir = "reports/completeness/")

    # Template used as a development version
    createreportfromdb("raw_flow_yearly", 440799, inputpath = "docs/development/completeness/", template = "completeness_dev.Rmd", encoding = "latin1", outputdir = "docs/development/completeness/")

    createreportfromdb("raw_flow_yearly", 440799,
                       template = "completeness.Rmd", outputdir = directory,
                       keep_tex = TRUE)
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

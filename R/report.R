#' Create data completeness report for comtrade data
#'
#' This function will generate reports for any template that is
#' product specific.  Giving information about all world trade
#' flows for one product.
#' @param tfdata a dataframe containing trade flows data
#' @param inputpath path of the template, defaults to package internal path
#' @param template name of the template file, including .Rmd extension
#' @param outputdir name of the output directory relative to getwd()
#' @param encoding, encoding of the template file. See also iconv
#' The names of encodings and which ones are available are
#' platform-dependent. All R platforms support ""
#' (for the encoding of the current locale), "latin1" and "UTF-8".
#' @export
createproductreport <- function(tfdata,
                                inputpath = system.file("templates",
                                                        package="tradeflows"),
                                template,
                                outputdir = "reports",
                                encoding = "UTF-8",
                                keep_tex = FALSE){
    # load optional packages
    require(ggplot2)
    require(reshape2)
    require(knitr)
    if (!"productcode" %in% names(tfdata)){
        stop("Rename raw data columns to EFI convention before running this function.")
    }

    # Don't show R code in output
    knitr::opts_chunk$set(echo=FALSE, warning=FALSE)

    # If the path is relative, make it an absolute path
    # So that rmarkdown render works properly
    if(substr(outputdir, 1,1) != "/"){
        outputdir <- file.path(getwd(), outputdir)
    }
    if (length(unique(tfdata$productcode)) != 1){
        message("There should be only one product in the trade flows data frame")
        return(FALSE)
    }
    productcodeinreport <- unique(tfdata$productcode)
    tryCatch(rmarkdown::render(input = file.path(inputpath, template),
                               output_format = rmarkdown::pdf_document(toc=TRUE,
                                                                       toc_depth = 3,
                                                                       keep_tex = keep_tex),
                               output_dir = outputdir,
                               output_file = paste0(productcodeinreport,".pdf"),
                               encoding = encoding),
             finally = print("Finally"))

}


#' Create report from the database
#' @description Create a report from the database
#' @rdname createrproducteport
#' @param template name of the template file
#' @param products a vector of product codes
#' @param ... arguments passed to \code{\link{createproductreport}()}
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
        createproductreport(tfdata = dtf, ...)
    }
}

#' Create reports for countries
#' Illustrating major trade flows.
createcountryreport <- function(countryinreport){

}

createcompletenessreport <- function()


if (FALSE){
    # You need to rebuild the package for template updates to take effect
    library(tradeflows)

    ###################### #
    # Completeness reports #
    ###################### #
    directory <- "docs/development/completeness/"
    # Template that will be exported with the package
    createreportfromdb("raw_flow_yearly", 440799, template = "completeness.Rmd",  outputdir = "docs/development/completeness/")

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

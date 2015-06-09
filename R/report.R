#' Extract report metadata from trade flow table
#' This can be used to check that the given trade flow data has only
#' one reporter and product or the empty string as expected by some reports.
#' @param tfdata a table of trade flows data
#' @return a list of metadata
extractmetadata <- function(tfdata){

}



#' Create data completeness report for comtrade data
#'
#' This function will generate reports for any template that is
#' product specific.  Giving information about all world trade
#' flows for one product.
#' @param tfdata a dataframe containing trade flows data
#' @param inputpath path of the template, defaults to package internal path
#' @param template name of the template file, including .Rmd extension
#' @param fileprefix character string at the begining of the generated pdf file name
#' @param productcode numeric or character string leave this variable empty if there are many products
#' @param reporter character string leave this variable empty if there are many reporters
#' @param outputdir name of the output directory relative to getwd()
#' @param encoding, encoding of the template file. See also iconv
#' The names of encodings and which ones are available are
#' platform-dependent. All R platforms support ""
#' (for the encoding of the current locale), "latin1" and "UTF-8".
#' @export
createproductreport <- function(tfdata,
                                template,
                                fileprefix = "",
                                productcode = "",
                                reporter = "",
                                inputpath = system.file("templates",
                                                        package="tradeflows"),
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
    # legacy check for a unitque product, report should be also generated for many products
    # if (length(unique(tfdata$productcode)) != 1){
    #         message("There should be only one product in the trade flows data frame")
    #         return(FALSE)
    #     }
    # Create the report file name
    filename <- paste0(fileprefix, productcode, reporter, ".pdf")
    tryCatch(rmarkdown::render(input = file.path(inputpath, template),
                               output_format = rmarkdown::pdf_document(toc=TRUE,
                                                                       toc_depth = 3,
                                                                       keep_tex = keep_tex),
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
#' or major discrepancies for one product
#' or major tradeflows for product group
createcountryreport <- function(countryinreport){
    # Convert country names could be a DB option in setdatabaseconfig
}

#' Create a discrepancy report
#'
#' @param productcode vector of product codes
#' @param reporter one single country name
#' @param ... arguments passed to \code{\link{createproductreport}()}
#' @examples
#'\dontrun{
#' creatediscrepancyreport(440799, "Cameroon",
#'     outputdir = "reports/discrepancies")
#' }
#' @export
creatediscrepancyreport <- function(productcode_, reporter_, ...){
    dtf <- readdbtbl("raw_flow_yearly") %>%
        filter(productcode == productcode_ &
                   (reporter == reporter_ | partner == reporter_)) %>%
        collect
    createproductreport(tfdata = dtf, template = "discrepancies.Rmd",
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

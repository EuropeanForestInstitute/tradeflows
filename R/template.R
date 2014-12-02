#' Create data completeness report for comtrade data
#'
#' You may want to generalise this function, for all template that are
#' product specific.
#' @param rawdata a dataframe of tradeflows data
#' @param products a vector of product codes
#' @param inputpath path of the template, defaults to package internal path
#' @param template name of the template file
#' @param outputdir name of the output directory relative to getwd()
#' @export
createcompletenessreport <- function(rawdata,
                                       products = "all",
                                       inputpath = system.file("templates",
                                                               package="tradeflows"),
                                       template = "completeness.Rmd",
                                       outputdir = "reports"){
    # load optional packages
    require(ggplot2)
    require(reshape2)
    require(knitr)
    if (!"productcode" %in% names(rawdata)){
        stop("Rename raw data columns to EFI convention before running this function.")
    }
    # Don't show R code in output
    knitr::opts_chunk$set(echo=FALSE, warning=FALSE)
    if (products=="all"){products <- unique(rawdata$productcode)}
    outputdir <- file.path(getwd(), outputdir)

    for (productcodeinreport in products){
        message("creating report for product: ",productcodeinreport)
        tryCatch(rmarkdown::render(input = file.path(inputpath, template),
                                   output_format = rmarkdown::pdf_document(toc=TRUE,
                                                                           toc_depth = 3),
                                   output_dir = outputdir,
                                   output_file = paste0(productcodeinreport,".pdf")),
                 finally = print("Finally"))
    }
}


if (FALSE){
    directory <- "docs/development/completeness/"
    # You need to reload the package for template updates to take effect
    createcompletenessreport(tradeflows::sawnwoodexample, outputdir = directory)
    # report for the "black hole" dataset
    load("data-raw/440799.RData")
    swd99 <- renamecolumns(dtf, "comtrade", "efi")
    createcompletenessreport(swd99, outputdir = directory)
    # another dataset
    load("data-raw/sawnwood_all.RData")
    swdall <- renamecolumns(swdall)
    create_completeness_report(swdall, "4407", outputdir = directory)
}

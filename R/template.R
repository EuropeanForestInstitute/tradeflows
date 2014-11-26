knitr::opts_chunk$set(echo=FALSE, warning=FALSE)
library(rmarkdown)
library(dplyr)


#' Create data completeness report for comptrade data
#'
#' You may want to generalise this function, for all template that are
#' product specific.
#' @param rawdata a dataframe of tradeflows data
#' @param products a vector of product codes
#' @param inputpath path of the template, defaults to package internal path
#' @param template name of the template file
#' @param outputdir name of the output directory relative to getwd()
#' @export
create_completeness_report <- function(rawdata,
                                       products = unique(rawdata$productcode),
                                       inputpath = system.file("templates",
                                                               package="tradeflows"),
                                       template = "completeness.Rmd",
                                       outputdir = "reports"){
    outputdir <- file.path(getwd(), outputdir)
    for (productcode_in_report in products){
        message("creating report for ",productcode_in_report)
        tryCatch(render(input = file.path(inputpath, template),
                        output_format = pdf_document(toc=TRUE,
                                                     toc_depth = 3),
                        output_dir = outputdir,
                        output_file = paste0(productcode_in_report,".pdf")),
                 finally= print("Finally"))
    }
}


if (FALSE){
    create_completeness_report(tradeflows::sawnwoodexample,
                               outputdir = "docs/development/products/reports/")

}

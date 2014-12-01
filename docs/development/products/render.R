library(knitr)
opts_chunk$set(echo=FALSE, warning=FALSE)
library(rmarkdown)
library(tradeflows)
library(dplyr)
library(reshape2)
library(ggplot2)

load("data/sawnwoodexample.rda")


############################################### #
# Outdated, see R/template.R for recent version #
############################################### #
#' Create product reports
#'
#' @param products
createreport_from_file <- function(products,
                         inputpath = "docs/development/products/",
                         template = "dev_completeness.Rmd",
                         outputdir = "reports"){
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



if(FALSE){
    rawdata <- sawnwoodexample %>% filter(reporter %in% c("France", "Germany"))
    createreport(4407)
    createreport(unique(rawdata$productcode))
}

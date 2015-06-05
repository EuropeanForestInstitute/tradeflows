# Create country reports from bilateral trade data
# Author: Paul Rougieux
library(knitr)
library(markdown)
library(lubridate)
library(dplyr)
library(reshape2)
library(xtable)
library(tools)
library(ggplot2)
# library(gridExtra)




knit_pdf <- function(flows, title, path="./docs/development/discrepancy/",
                     template="country.Rnw",
                     output_path = paste0(path,"countries/")){
  #' @description Create a pdf report based on the Rnw template
     #' @param flows dataframe containing trade flows data for a country and a period
     #' @param titre title of the report
     #'
     project_dir <- getwd()
     file.copy(paste0(path, template),
               paste0(path,"draft/template.Rnw"),
               overwrite=TRUE)

     # Create the tex file
     setwd(paste0(path,"draft/"))
     tryCatch(knit("template.Rnw"),
              finally= setwd(project_dir))

     # Create the PDF file
     setwd(paste0(path,"draft/"))
     tryCatch(texi2pdf("template.tex"),
              finally= setwd(project_dir))
     print(path)
     print(output_path)
     # Copy PDF to the output folder
     file.copy(paste0(path,"draft/template.pdf"),
               paste0(output_path,title,".pdf"),
               overwrite=TRUE)
}


if(FALSE){
    # create country reports
    ########### #
    # Roundwood #
    ########### #
    dtf <- readdbproduct(440799, "validated_flow_yearly")
    # Hack, because tradevaluepartner is missing from the database
    dtf <- dtf %>%
        mutate(quantitypartnerbackup = quantitypartner) %>%
        select(-quantitypartner) %>%
        addpartnerflow %>% calculatediscrepancies
    flows <- dtf %>% filter(reporter == "France")
    knit_pdf(subset(dtf, reporter=="France"), "France Trade flows",
             template = "discrepancy.Rnw")

    # Other reports
    knit_pdf(subset(rwd,reporting_country=="France"), "France roundwood trade flows")
    knit_pdf(subset(rwd,reporting_country=="Germany"), "Germany roundwood trade flows")
    knit_pdf(subset(rwd,reporting_country=="Cameroon"), "Cameroon roundwood trade flows")
    knit_pdf(subset(rwd,reporting_country=="United States of America"), "United States roundwood trade flows")
    knit_pdf(subset(rwd,reporting_country=="Belgium"), "Belgium roundwood trade flows")
    knit_pdf(subset(rwd,reporting_country=="China"), "China roundwood trade flows")
    knit_pdf(subset(rwd,reporting_country=="Togo"), "Togo roundwood trade flows")


    ########## #
    # Sawnwood #
    ########## #
    knit_pdf(subset(swd,reporting_country=="France"), "France sawnwood trade flows")
    knit_pdf(subset(swd,reporting_country=="Belgium"), "Belgium sawnwood trade flows")
    knit_pdf(subset(swd,reporting_country=="Japan"), "Japan sawnwood trade flows")
    knit_pdf(subset(swd,reporting_country=="China"), "China sawnwood trade flows")
    knit_pdf(subset(swd,reporting_country=="Cameroon"), "Cameroon sawnwood trade flows")
    knit_pdf(subset(swd,reporting_country=="United States of America"), "USa Sawnwood trade flows")
    knit_pdf(subset(swd,reporting_country=="Viet Nam"), "Vietnam Sawnwood trade flows")

    # !!! This has to be moved to an Excel file !!!
    # Create global reports
    knit_pdf(subset(rwd,year%in%c(2001,2002)), "Global discrepancies in Roundwood trade",
             template="global.Rnw", output_path = "docs/reports/global/")

}

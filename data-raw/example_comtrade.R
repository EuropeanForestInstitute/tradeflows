################################### #
# Load Example dataset for sawnwood #
################################### #
# Load from local database
sawnwoodexample <- readdbproduct(440799, "raw_flow_yearly")
# Save data frame for dissemination within the package
devtools::use_data(sawnwoodexample, overwrite = TRUE)

# Alternative (long) load from Comtrade API
# loadcomtradeallreporters(440799, path = "/tmp")
# load("/tmp/440799.RData")
# sawnwoodexample <- dtf %>% renamecolumns()
# More details on API issues in "docs/development/comtrade.Rmd"


# Call functions from load.R to load sample comtrade datasets
# No need to source load.R as it is loaded within the package already
# This script will run on a development system
# On a production system raw data might be storred in a database
#
# Development Issues with JSON parsing were explained under:
# docs/development/input.Rmd
# Examples under "docs/development/comtrade.Rmd"
# country specific issues under "docs/development/countries"
#
# Example of an R script that loads raw data:
# https://github.com/hadley/nycflights13/blob/master/data-raw/airlines.R
library(dplyr)


# Country codes and names
# View(tradeflows::reportercomtrade)

# Product codes and names in chapter 44
# View(filter(tradeflows::classificationcomtrade$HS,
#                 substr(productcode, 0, 2)=="44"))

# Product code and names under sawnwood 4407
claswd <- classificationcomtrade$H4 %>%
    filter(substr(productcode, 0, 4)=="4407")
claswd$productcode
nrow(claswd)




################ #
# Other datasets #
################ #
if (FALSE){
    # UK wood pellets
    pelletsuk <- loadcomtrade_bycode(440131, 826, "recent")
    save(pelletsuk, file="data-raw/pelletsexample.RData")

    # The rest loads trade datasets
    # for all countries in reportercomtrade
    # This is only on a development system
    # Not exported within the package
    # Loading all datasets will be done on a server

    # Save tradeflows to RDATA
    # One file by product at 4 digit level
}

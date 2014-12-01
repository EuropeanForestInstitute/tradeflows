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


################################### #
# Load Example dataset for sawnwood #
################################### #
# More details on API issues in "docs/development/comtrade.Rmd"
# Germany, swd, swdoak, swdbeech
swdde <- loadcomtrade_bycode(c(4407, 440791, 440792), 276, "recent")
# add 5 years before
swdde2 <- loadcomtrade_bycode(c(4407, 440791, 440792), 276, seq(2004,2008))
swdde <- rbind(swdde, swdde2)
# France, swd, swdoak, swdbeech
swdfr <- loadcomtrade_bycode(c(4407, 440791, 440792), 251, "recent")
# add 5 years before
swdfr2 <- loadcomtrade_bycode(c(4407, 440791, 440792), 251, seq(2004,2008))
swdfr <- rbind(swdfr, swdfr2)

# bind tables together
sawnwoodexample <- rbind(swdde, swdfr) %>%
    arrange(yr, rgCode, ptCode)
unique(sawnwoodexample[c("yr", "rtTitle")])
sawnwoodexample <- renamecolumns(sawnwoodexample)
devtools::use_data(sawnwoodexample, overwrite = TRUE)


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

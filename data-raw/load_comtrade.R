# Load product codes and names in the different classifications
# Load reporter area codes and names (mostly country names)
# Call functions from load.R to load sample comtrade datasets
#
# No need to source load.R as it is loaded within the package already
# This script will run on a development system
# On a production system raw data might be storred in a database
#
# Development Issues with JSON parsing were explained under:
# docs/development/input.Rmd
#
# Example of file that load data:
# https://github.com/hadley/nycflights13/blob/master/data-raw/airlines.R
library(dplyr)




###################### #
# Load Example dataset #
###################### #

# Country codes and names
# View(tradeflows::reportercomtrade)

# Product codes and names in chapter 44
# View(filter(tradeflows::classificationcomtrade$HS,
#                 substr(productcode, 0, 2)=="44"))

# More examples under "docs/development/comtrade.Rmd"
# And country specific issues under "docs/development/countries"
claswd <- classificationcomtrade$H4 %>%
    filter(substr(productcode, 0, 4)=="4407")
claswd$productcode


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


# uk wood pellets
pelletsuk <- loadcomtrade_bycode(440131, 826, "recent")


# bind tables
sawnwoodexample <- rbind(swdde, swdfr) %>%
    arrange(yr, rgCode, ptCode)
unique(sawnwoodexample[c("yr", "rtTitle")])
save(sawnwoodexample, file="data-raw/sawnwoodexample.RData")
save(pelletsuk, file="data-raw/pelletsexample.RData")

# The rest loads trade datasets
# for all countries in reportercomtrade
# This is only on a development system
# Not exported within the package
if (FALSE){
    # Save tradeflows to RDATA
    # One file by product at 4 digit level
}

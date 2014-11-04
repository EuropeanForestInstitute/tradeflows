# This will call functions from load.R to load comtrade data
#
# No need to source load.R as it is loaded within the package already
# This script will run on a development system
# On a production system raw data might be storred in a database
#
# Issues with JSON parsing are explained under:
# docs/development/input.Rmd
#
# Example of file that load the data:
# https://github.com/hadley/nycflights13/blob/master/data-raw/airlines.R
# Comtrade reporter areas
# http://comtrade.un.org/data/cache/reporterAreas.json
# Keep only countries for which there is sawnwood or fuelwood data in 2010
# This should eliminate former countries such as east germany


# Comtrade Classifications
# H4
# http://comtrade.un.org/data/cache/classificationH4.json


years = seq(2000,2012)

## Sawnwood Germany
swd2012 <- loadcomtrade_bycode(4407, 276, 2012)
swd20102011 <- loadcomtrade_bycode(4407, 276, c(2010, 2011))
swd20002013 <- loadcomtrade_bycode(4407, 276, seq(2008,2012))
sawnwood <- loadcomtrade_bycode(4407, 276, "recent")

# Save tradeflows to RDATA
# There will be one file by product
save(sawnwood, fil="data-raw/s")

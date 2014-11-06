# This will call functions from load.R to load comtrade data
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
#
# Comtrade reporter areas
# http://comtrade.un.org/data/cache/reporterAreas.json
# Keep only countries for which there is sawnwood or fuelwood data in 2010
# This should eliminate former countries such as east germany



# Comtrade Classifications
# H4
# http://comtrade.un.org/data/cache/classificationH4.json


## Sawnwood Germany
# More examples under "docs/development/input.Rmd"
sawnwood <- loadcomtrade_bycode(4407, 276, "recent")


# Save tradeflows to RDATA
# One file by product
save(sawnwood, file="data-raw/sawnwood.RData")




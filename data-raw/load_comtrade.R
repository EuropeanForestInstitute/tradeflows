# This will call functions from load.R, no need to source that file as it is loaded within the package already
# to actually load the data from comtrade
# This script will run on a development system
# On a production system raw data might be storred in a database


# Comtrade reporter areas
# http://comtrade.un.org/data/cache/reporterAreas.json
# Keep only countries for which there is sawnwood or fuelwood data in 2010
# This should eliminate former countries such as east germany


# Comtrade Classifications
# H4
# http://comtrade.un.org/data/cache/classificationH4.json



swd2012 <- loadcomtrade_bycode(4407, 276, 2012)
swd2010 <- loadcomtrade_bycode(4407, 276, 2010)
swdrecent <- loadcomtrade_bycode(4407, 276, "recent")

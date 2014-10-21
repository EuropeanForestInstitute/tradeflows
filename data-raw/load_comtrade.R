# This will call functions from load.R
# to actually load the data from comtrade
# This script will run on a development system
# On a production system raw data might be storred in a database
# With column names corresponding to the FAOSTAT names

require(RJSONIO)
require(dplyr)

# Figure out  how to download many years at once
swd <- loadcomtrade_bycode(4407, 276, 2013)
swd2012 <- loadcomtrade_bycode(4407, 276, 2012)

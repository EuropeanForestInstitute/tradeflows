# Prepare the reporter names and codes reference tables for use
# within the package
library(dplyr)

########################### #
# Comtrade reporter areas   #
########################### #
# http://comtrade.un.org/data/cache/reporterAreas.json
# Keep only countries for which there is sawnwood or fuelwood data in 2010
# This should eliminate former countries such as east germany
reportercomtrade <- jsonlite::fromJSON("http://comtrade.un.org/data/cache/reporterAreas.json")
reportercomtrade <- reportercomtrade$results %>% select(reportercode = id,
                                                reporter = text)
devtools::use_data(reportercomtrade, overwrite = TRUE)


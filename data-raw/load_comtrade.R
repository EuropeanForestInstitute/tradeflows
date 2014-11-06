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

load_comtrade_definition <- function(url){
    jsonfile <- tempfile(fileext = ".json")
    download.file(url, destfile=jsonfile)
    json <- jsonlite::fromJSON(jsonfile)
    unlink(jsonfile)
    return(json$results)
}

########################### #
# Comtrade reporter areas   #
########################### #
# http://comtrade.un.org/data/cache/reporterAreas.json
# Keep only countries for which there is sawnwood or fuelwood data in 2010
# This should eliminate former countries such as east germany
reportercomtrade <-
    load_comtrade_definition("http://comtrade.un.org/data/cache/reporterAreas.json")
save(reportercomtrade, file="data-raw/reportercomtrade.RData")


########################### #
# Comtrade Classifications  #
########################### #
# H4
# http://comtrade.un.org/data/cache/classificationH4.json
classificationH4 <- load_comtrade_definition("http://comtrade.un.org/data/cache/classificationH4.json")
save(classificationH4, file="data-raw/classificationH4.RData")
classificationHS <- load_comtrade_definition("http://comtrade.un.org/data/cache/classificationHS.json")
save(classificationHS, file="data-raw/classificationHS.RData")

# You may want to save all classification dataframes in one list

# HS not in H4
HSnotinH4 <- classificationHS %>%
    filter(!id %in% classificationH4$id & substr(id, 0, 2)=="44")
H4notinHS <- classificationH4 %>%
    filter(!id %in% classificationHS$id & substr(id, 0, 2)=="44")


###################### #
# Load Sawnwood data   #
###################### #
# More examples under "docs/development/comtrade.Rmd"
swd <- classificationH4 %>% filter(substr(id, 0, 4)=="4407")

# Germany
sawnwood <- loadcomtrade_bycode(4407, 276, "recent")
# France
swdfr <- loadcomtrade_bycode(4407, 251, "recent")
swdoakfr <- loadcomtrade_bycode(440791, 251, "recent")


# Save tradeflows to RDATA
# One file by product at 4 digit level
save(sawnwood, file="data-raw/sawnwood.RData")




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
HS <- load_comtrade_definition("http://comtrade.un.org/data/cache/classificationHS.json")
url <- "http://comtrade.un.org/data/cache/classification" # fancy
H4 <- load_comtrade_definition(paste0(url,"H4",".json"))
classificationcomtrade <- list(HS = HS, H4 = H4)
save(classificationcomtrade, file="data-raw/classificationcomtrade.RData")

###################### #
# Load Sawnwood data   #
###################### #
# More examples under "docs/development/comtrade.Rmd"
# And country specific issues under "docs/development/countries"
claswd <- classificationcomtrade$H4 %>% filter(substr(id, 0, 4)=="4407")
claswd$id
# Germany
sawnwood <- loadcomtrade_bycode(4407, 276, "recent")
# France
swdfr <- loadcomtrade_bycode(4407, 251, "recent")
swdoakfr <- loadcomtrade_bycode(440791, 251, "recent")
swdfr <- loadcomtrade_bycode(c(440791, 440792), 251, "recent")
sawnwood <- rbind(sawnwood, swdfr, swdoakfr)

# Save tradeflows to RDATA
# One file by product at 4 digit level
save(sawnwood, file="data-raw/sawnwood.RData")




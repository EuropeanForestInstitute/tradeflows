# Load product codes and names for the different classifications
# Prepare the product classification reference tables for use
# within the package
library(dplyr)

############################# #
# ITTO product classification #
############################# #
itto <- read.csv("data-raw/ITTO_products.csv")


########################### #
# Comtrade Classifications  #
########################### #
url <- "http://comtrade.un.org/data/cache/classification"
HS <- jsonlite::fromJSON(paste0(url,"HS",".json"))
H4 <- jsonlite::fromJSON(paste0(url,"H4",".json"))
H3 <- jsonlite::fromJSON(paste0(url,"H3",".json"))
H2 <- jsonlite::fromJSON(paste0(url,"H2",".json"))
H1 <- jsonlite::fromJSON(paste0(url,"H1",".json"))
# If all ok
stopifnot(nrow(H4$results)> 1000)
classificationcomtrade <- list(HS = HS$results,
                               H4 = H4$results,
                               H3 = H3$results,
                               H2 = H2$results,
                               H1 = H1$results)
classificationcomtrade <- lapply(classificationcomtrade,
                                 select,
                                 productcode = id,
                                 product = text,
                                 parentcode = parent)


# Keep only certain chapters
# 44 wood products and 94 furniture
# Add other chapters later as necessary
keep_codes_starting_with <- c("44", "94") # list of 2 digit codes
classificationcomtrade <- lapply(classificationcomtrade,
                                 filter,
                                 substr(productcode, 0, 2) %in%
                                     keep_codes_starting_with)

devtools::use_data(classificationcomtrade, overwrite = TRUE)


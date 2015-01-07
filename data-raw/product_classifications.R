# Load product codes and names for the different classifications
# Prepare the product classification reference tables for use
# within the package
library(dplyr)

############################# #
# ITTO product classification #
############################# #
classificationitto <- read.csv("data-raw/classificationitto.csv", as.is=TRUE)
classificationitto <- classificationitto %>%
    select(product = Names.of.products,
           productcodeitto = Product.code,
           # Names are different than in Comtrade API
           nomenclature = Nomenclature,
           productcodecomtrade = Code,
           description = Description,
           exceptions = Exceptions,
           tropical = Tropical)
# Change tropical column to TRUE / FALSE
unique(classificationitto$tropical)
classificationitto$tropical[c(classificationitto$tropical=="X")] <- TRUE
classificationitto$tropical[c(classificationitto$tropical=="")] <- FALSE
classificationitto$tropical <- as.logical(classificationitto$tropical)
stopifnot(sum(is.na(classificationitto$tropical))==0)

# Unique chapter
# classificationitto$chapter <- round(classificationitto$productcodecomtrade/10000)
# unique(classificationitto$chapter)
# Extract table for methodology report
jfsq <- classificationitto %>%
    # Filter logs and sawnwood
    mutate(code4 = round(productcodecomtrade/100)) %>%
    filter(code4 %in% c(4407,4404)) %>%
    select(product, jfsqcode  = productcodeitto, productcodecomtrade) %>%
    unique
print.xtable(jfsq)#,include.rownames=FALSE, type = "latex", floating=FALSE)

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

################################################# #
# Save product classifications as package objects #
################################################# #
devtools::use_data(classificationcomtrade,
                   classificationitto,
                   overwrite = TRUE)


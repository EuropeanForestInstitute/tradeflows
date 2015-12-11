# Load product codes and names for the different classifications
# Prepare the product classification reference tables for use
# within the package
library(dplyr)
library(xtable)
############################# #
# ITTO product classification #
############################# #
library(tradeflows)
# Use of detach is best avoided in function, according to the help file.
detach(package:tradeflows) # Make sure the classificationitto is not there

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
# Logssawnood <- classificationitto %>%
#     # Filter logs and sawnwood
#     mutate(code4 = round(productcodecomtrade/100)) %>%
#     filter(code4 %in% c(4407,4404)) %>%
#     select(product, jfsqcode  = productcodeitto, productcodecomtrade) %>%
#     unique
#print.xtable(jfsq)#,include.rownames=FALSE, type = "latex", floating=FALSE)

# As requested for the overview report in September 2015
# Add JFSQ 1 and JFSQ 2 codes and names to classificationitto
jfsq <- read.csv("data-raw/ittoproducts_jfsq.csv", stringsAsFactors = FALSE) %>%
    select(jfsq1code = JFSQ.1,
           jfsq2code = JFSQ.2,
           productcodecomtrade = HSFULL6D)
# Add missing data
# See bug report: http://193.185.149.20/redmine/issues/143
if (!480230 %in% jfsq$productcodecomtrade){
    jfsq <- jfsq %>%
        rbind(data_frame(jfsq1code = 12,
                         jfsq2code = 12.6,
                         productcodecomtrade = 480230))
}

jfsq1 <- read.csv("data-raw/ittoproducts_jfsq1_names.csv", stringsAsFactors = FALSE) %>%
    rename(jfsq1code = JFSQ_codes,
           jfsq1name = JFSQ_names)
jfsq2 <- read.csv("data-raw/ittoproducts_jfsq2_names.csv",stringsAsFactors = FALSE) %>%
    rename(jfsq2code = JFSQ_codes,
           jfsq2name = JFSQ_names)

# Change names to an ordered factor
jfsq1$jfsq1name <- factor(jfsq1$jfsq1name,
                               levels = jfsq1$jfsq1name)
jfsq2$jfsq2name <- factor(jfsq2$jfsq2name,
                               levels = jfsq2$jfsq2name)

# Merge comtrade codes with jfsq names
jfsq1 <- jfsq %>%
    select(jfsq1code, productcodecomtrade) %>%
    left_join(jfsq1, by=c("jfsq1code"))
jfsq2 <- jfsq %>%
    select(jfsq2code, productcodecomtrade) %>%
    left_join(jfsq2, by = "jfsq2code")

# Issue with 441129 beeing present twice
jfsq1 <- jfsq1 %>% filter(!(productcodecomtrade == 441129 & jfsq1name == "SECONDARY PAPER PRODUCTS"))
jfsq2 <- jfsq2 %>% filter(!(productcodecomtrade == 441129 & jfsq2name == "SPECIAL COATED PAPER AND PULP PRODUCTS"))

# Check uniqueness of productcodes
jfsq1unique <- jfsq1 %>% group_by(productcodecomtrade) %>%
    summarise(n=n()) %>% arrange(desc(n))
stopifnot(sum(jfsq1unique$n) ==nrow(jfsq1unique))
jfsq2unique <- jfsq2 %>% group_by(productcodecomtrade) %>%
    summarise(n=n()) %>% arrange(desc(n))
stopifnot(sum(jfsq2unique$n) ==nrow(jfsq2unique))

# Merge with itto classification
classificationitto <- classificationitto %>%
    full_join(jfsq1, by = "productcodecomtrade") %>%
    full_join(jfsq2, by = "productcodecomtrade")

# is 440210 in there? itto doesn't want it as wood charcoal
classificationitto %>% filter(productcodecomtrade == 440210)

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
                                 description = text,
                                 parentcode = parent)


# Keep only certain chapters
# 44 wood products and 94 furniture, 47 and 48.
# Add other chapters later as necessary
keep_codes_starting_with <- c("44", "94", "47", "48") # list of 2 digit codes
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


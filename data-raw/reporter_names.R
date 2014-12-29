# Load reporter area codes and names (mostly country names)
# Prepare the reporter names and codes reference tables for use
# within the package
library(dplyr)
library(FAOSTAT)


################################ #
# Load comtrade reporter areas   #
################################ #
# http://comtrade.un.org/data/cache/reporterAreas.json
# Keep only countries for which there is sawnwood or fuelwood data in 2010
# This should eliminate former countries such as east germany
reportercomtrade <- jsonlite::fromJSON("http://comtrade.un.org/data/cache/reporterAreas.json")
reportercomtrade <- reportercomtrade$results %>%
    select(reportercode = id, reporter = text) %>%
    filter(reportercode != "all") # Remove "all" as it is not allowed for query complexity reasons
nbcountriesincomtrade <- nrow(reportercomtrade)

############################## #
# Add ITTO regional aggregates #
############################## #
ittoregions <- read.csv("data-raw/ITTO_reporters.csv", as.is=TRUE)
# rename columns
ittoregions <- ittoregions %>%
    select(reportercode = COMTRADE.CODE,
           reporter = COMTRADE.COUNTRIES,
           region = Regional.Distribution)
names(ittoregions)[grepl("tropical",names(ittoregions))] <- "tropical"
reportercomtrade <- merge(reportercomtrade,
                           select(ittoregions, reportercode, region),
                           all.x=TRUE)


###################################### #
# Load regional aggregates from FAOSTAT #
###################################### #
FAOcountryProfile2 <- FAOcountryProfile %>%
    select(reportercodefao = FAOST_CODE,
           reporteriso = ISO3_CODE,
           reporternamefao = FAO_TABLE_NAME)

regions <- FAOregionProfile %>%
    filter(!is.na(FAOST_CODE)) %>%
    select(reportercode = UN_CODE,
           reportercodefao = FAOST_CODE,
           regionfao = UNSD_MACRO_REG,
           subregion = UNSD_SUB_REG) %>%
    merge(FAOcountryProfile2, by="reportercodefao") %>%
    arrange(regionfao, subregion) %>%
    # Remove the reportercode as this is not part of the merge
    select(-reportercode)

# Add an isocode for china
# Remove the CHN iso so that there is no duplicated iso code
regions$reporteriso[regions$reporteriso=="CHN"] <- NA
# reportercodefao 352 is the official
# China (China mainland, Hong Kong SAR, Macao SAR, Taiwan)
regions$reporteriso[regions$reportercodefao==351]  <- "CHN"


########################################### #
# Add regional aggregates to comtrade table #
########################################### #
# See docs/development/regions.Rmd for a struggle
# with regional aggregation
## Merge FAO regions based on reporteriso and ISO3_CODE
# Load the sawnwood dataset and get iso codes from there
load("data-raw/comtrade/4407.RData")
reportercomtrade <- dtf %>%
    renamecolumns %>%
    select(reportercode = partnercode,
           reporter = partner,
           reporteriso = partneriso) %>%
    unique %>%
    # Merge based on iso3 code, remove NA and duplicates (for example SDN)
    merge(filter(regions, !is.na(reporteriso) & !duplicated(reporteriso)),
          by="reporteriso") %>%
    merge(reportercomtrade, all.y = TRUE)
nrow(reportercomtrade)
reportercomtrade$reporter[!reportercomtrade$reporter %in%
                              tradeflows::reportercomtrade$reporter]
length(unique(reportercomtrade$reporter))
reportercomtrade[duplicated(reportercomtrade$reporter),]
nbcountriesincomtrade

# Remove duplicates
reportercomtrade %>% filter(!duplicated(reportercode))

# reportercomtrade$reporteriso[duplicated(reportercomtrade3$reporteriso)]


## Check where region fao and ITTO are different


#################################################### #
# Save matching table for further use in the package #
#################################################### #
devtools::use_data(reportercomtrade, overwrite = TRUE)


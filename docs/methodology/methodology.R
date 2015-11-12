#' R output for the methodology report Lyx file
#'
library(tradeflows)
library(dplyr)
library(xtable)
library(ggplot2)
library(reshape2)

#' Print data frame in latex format
#' @param dtf data frame
#' @param label table lable, see xtable
#' @param caption
printtable <- function(dtf, label=NULL, caption=NULL, align=NULL){
    latextable <-xtable(dtf, digits = 0, align = align,
                        label = label, caption = caption)
    if (!is.null(align)){
        # Table with full width in tabularx environment
        print.xtable(latextable,
                     include.rownames=FALSE, type = "latex", floating=TRUE,
                     tabular.environment = "tabularx",
                     width="\\textwidth")
    } else {
        # basic table
        print.xtable(latextable,
                     include.rownames=FALSE, type = "latex", floating=TRUE)
    }

}


#### Conversion factors ####
# Absolute path for execution within lyx
load("/home/paul/R/tradeflows/data-raw/comtrade/440799.RData")
swd99 <- dtf %>% renamecolumns %>%
    filter(year %in% c(2010,2011)) %>%
    removeduplicatedflows %>%
    addconversionfactorandprice %>%
    addregion
swd99conv <- swd99 %>% extractconversionfactors %>%
    arrange(year,flow) %>% select(-unit) %>% data.frame
swd99price <- swd99 %>% extractprices %>% select(-unit) %>% data.frame


#### Product codes ####
jfsqcomtrade <- classificationitto %>%
    # Filter logs and sawnwood
    # filter(product %in% c("LOGS","SAWNWOOD")) %>%
    # The filter instruction is commented out because we
    # Select all nomenclatures in the itto tables
    # Deal with duplicates with the unique() statement at the end
    # filter(nomenclature=="HS12") %>%
    select(product, productcodeitto, productcodecomtrade, tropical) %>%
    merge(classificationcomtrade$HS, by.x="productcodecomtrade", by.y="productcode") %>%
    mutate(description = substring(description,10)) %>%
    mutate(description = gsub("/","/ ",description),
           description = gsub("cm³","cm^3", description),
           product = gsub("’","",product)) %>%
    select(Product = product, JFSQ = productcodeitto,
           Comtrade = productcodecomtrade,
           # Tropical = tropical,
           Description = description) %>%
    unique %>%
    arrange(JFSQ) #%>% kable(row.names=FALSE)


swd99region <- swd99 %>%
    filter(flow %in% c("Import", "Export")) %>%
    group_by(flow, regionreporter, year) %>%
    summarise(lowerprice = round(0.5 * quantile(price, 0.25,
                                                names=FALSE, na.rm=TRUE)),
              medianprice = round(median(price, na.rm=TRUE)),
              upperprice = round(2 * quantile(price, 0.75,
                                              names=FALSE, na.rm=TRUE)),
              priceregion = round(sum(tradevalue) / sum(quantity))) %>%
    arrange(-medianprice)


########################## #
### Price distribution ### #
########################## #
# Plot trade value distribution and median trade value by region
# Calculate total trade value by region, year and show total trade value

pricedistri <- swd99 %>%
    mutate(pricegroup = round(price/100) * 100) %>%
    filter(flow %in% c("Import", "Export")) %>%
    group_by(regionreporter, flow, year, pricegroup, flag) %>%
    summarise(quantity = sum(quantity),
              tradevalue = sum(tradevalue),
              number = n()) %>%
    data.frame() %>% # Because mutate to create a factor on a grouped data frame creates an error
    mutate(flag = as.factor(flag)) %>% #
    filter(!is.na(pricegroup) & pricegroup!=Inf)


pricedistriagg <- pricedistri %>%
    filter(year == 2011) %>%
    group_by(pricegroup, flow, flag) %>%
    summarise(quantity = sum(quantity),
              tradevalue = sum(tradevalue),
              number_of_flows = sum(number)) %>%
    melt(id = c("pricegroup", "flow", "flag"))

# Error with fill = flag, when flag contains NA values.
pricedistriplot <- ggplot(pricedistri,
                         aes(x = pricegroup, y = quantity/1e6, fill = flag)) +
#     geom_point() + # usefull to see what was wrong with xlim
    geom_bar(stat="identity") +
    ylab(expression("Trade volume in million m"^3))  +
    xlim(-100,1500) + xlab("Price in $")
# pricedistriplot + facet_grid(flow~.) + ggtitle("World trade flows for 440799") #+ aes(fill=region)
# message("separate by flag")
# pricedistriplot + aes(y = tradevalue) + facet_grid(flow~.)

pricedistriaggplot <- ggplot(pricedistriagg,
                             aes(x= pricegroup, y = value, fill = flag)) +
    geom_bar(stat= "identity") +
    xlim(-100,1500) + xlab("Price in $") +
    ylab("") +
    facet_grid(variable ~ flow, scales = "free_y")


# Flag issue
swd99 %>% group_by(flag, flow) %>% summarise(tradevalue = sum(tradevalue))

# pricedistri + ggtitle("Total trade value for 440799 by region and year
#    Vertical red lines represent median unit prices (by number of flows for each region),
#    Vertical green lines represent aggregate unit price for each region") +
#     geom_vline(data=swd99region, aes(xintercept = medianprice), colour="red") +
#     geom_vline(data=swd99region,
#                aes(xintercept = priceregion), colour="green") +
#     facet_grid(regionreporter + flow ~ year, scales = "free_y")

message("Change this for total trade volume at a given price and delete this message")
message("Maybe the plot in total trade value was good after all")
# At least it had a higher values for the red part of the plot

if (FALSE){
    # Begining of
    classificationitto %>% filter(productcodecomtrade==440320)

    # Tables used in lyx
    pricetable <- swd99price %>%
        select(Region = regionreporter, Year = year, Flow = flow,
               Lower = lowerprice, Median = medianprice, Upper = upperprice,
               Weighted_Average = weightedaverageprice) %>%
        arrange(Region, Year, Flow)

    printtable(pricetable,
               label = "tab:swd99price",
               caption = "Lower, median, upper and weighted average prices of other sawnwood by region")

    printtable(swd99price,
               label = "tab:swd99price",
               caption = "Median prices of other sawnwood by region")

    printtable(swd99conv,
               label = "tab:swd99conv",
               caption = "Median conversion factor of other sawnwood for all world trade flows")

    printtable(filter(jfsqcomtrade, Product=="LOGS"),
               label = "tab:logsjfsqcomtrade",
               caption = "JFSQ and Comtrade codes for logs",
               align = "|l|l|l|l|X|")

    printtable(filter(jfsqcomtrade, Product=="SAWNWOOD"),
               label = "tab:swdjfsqcomtrade",
               caption = "JFSQ and Comtrade codes for sawnwood",
               align = "|l|l|l|l|X|")

    # Plots used in lyx
    pricedistriplot + ggtitle("") +
        geom_vline(data=swd99region, aes(xintercept = medianprice), colour="red") +
        geom_vline(data=swd99region,
                   aes(xintercept = priceregion), colour="green") +
        facet_grid(regionreporter  ~ flow + year, scales = "free_y")

    pricedistriplot + ggtitle("") +
        facet_grid(  ~ flow + year, scales = "free_y")

    ############################### #
    # germany overview sample plot ####
    ############################### #
    tfdata <- createoverviewreport("Germany", dataonly = TRUE, jfsqlevel = 2)
    # After a source of the function in inst/template/overviewquantity.Rmd
    # filterlargepartners() and largepartnersbyflow() become available
    # here I try to avoid using them therfore the micmac
    logsde0 <- rbind(filterlargepartners(tfdata,"LOGS", "Volume in cubic meters","Import"),
                     filterlargepartners(tfdata,"LOGS", "Volume in cubic meters","Export"))
    # JFSQ level on products
    logs <- classificationitto %>% filter(product=="LOGS") %>%
        select(productcodecomtrade) %>% distinct()
    dput(logs$productcodecomtrade)
    # These are the largest import and export partners respectively
    dput(largepartnersbyflow(tfdata,"LOGS", "Volume in cubic meters","Import")$partnercode)
    dput(largepartnersbyflow(tfdata,"LOGS", "Volume in cubic meters","Export")$partnercode)

    # Copy from here
    # Select major log trade flows in Germany
    logsde <- readdbtbl("validated_flow_yearly") %>%
        filter(productcode %in% c(440320L, 440331L, 440332L, 440333L, 440334L, 440335L, 440341L,
                                  440349L, 440391L, 440392L, 440399L) &
                   reporter == "Germany" &
                   ((flow == "Import" & partnercode %in% c(203, 616, 251, 752, 56)) |
                        flow == "Export" & partnercode %in% c(40, 156, 381, 56, 251))) %>%
        collect()

    #' Plot product, to be used in the loop on product names below
    #' For the moment tfdata is taken from the global environment is this bad?
    #' @param product_ the itto name of a product
    productplot <- function(product_, unit_, reporterinreport){
        # Check if the given product is in the tfdata data frame
        stopifnot(product_ %in% unique(tfdata$product))
        import <- filterlargepartners(tfdata, product_, unit_, "Import")
        export <- filterlargepartners(tfdata, product_, unit_, "Export")
        combined <- rbind(import, export)
        # test if there are more than one unit, give a warning
        p <- ggplot(NULL,
                    aes(x = year, y = tradevalue,
                        fill = productcode)) +
            # geom_bar(stat="identity") +
            ylab(paste(unique(combined$unit))) +
            theme(legend.position= "bottom") +
            # Scale might be changed to avoid year overlapping when there are many years
            # scale_x_continuous(breaks = c(2010,2012)) +
            facet_grid(flow + reporter ~ partner, scales="free_y") +
            guides(fill=guide_legend(nrow=2,byrow=TRUE))
        # One plot for import and one plot for export
        importp <- p + geom_bar(data = import,
                                stat="identity") +
            ggtitle(paste("Largest ", product_, " imports reported by", reporterinreport))
        exportp <- p + geom_bar(data = export,
                                stat="identity") +
            ggtitle(paste("Largest ", product_, " exports reported by", reporterinreport))
        try(print(importp))
        try(print(exportp))
        # Description call has to be placed here
        # because the combined dataframe is created in this function
        try(description(as.character(unique(combined$productcode))))
    }
    logsde$product <- "LOGS"
    productplot("LOGS",  "Volume in cubic meters", "Germany")
}

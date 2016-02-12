### To start the server and view it in your web browser:
# library(shiny)
# runApp("docs/1_explore/shiny")
# To start the server and view it in Rstudio's viewer pane:
# runApp("docs/1_explore/shiny", launch.browser = rstudio::viewer)

### To start the server from a shell prompt
# Rscript -e 'library(methods); shiny::runApp("docs/1_explore/shiny", launch.browser=TRUE)'
# The expression -e 'library(methods)' is needed because of a missing dependency
# https://github.com/hadley/testthat/issues/122
# https://groups.google.com/forum/#!topic/shiny-discuss/Gx2P_dhzM38

### To deploy this app
# library(shinyapps)
# shinyapps::deployApp('path/to/your/app')
# But dataset need to be placed in the same directory
# Or maybe I could use symbolic links here?

### Further development ideas
# Example of scripts that download a knitr report from shiny
# http://shiny.rstudio.com/gallery/download-knitr-reports.html

library(shiny)
library(ggplot2)
library(dplyr)
library(tradeflows)
# Set path to root of project
# if (grepl("docs/1_explore/shiny",getwd())) setwd("../../..")
warning("server.R getwd:",getwd())
# Load data in the same directory as server.R,
# .RData files are symbolic links
readdbtbl("validated_flow_yearly")


shinyServer(function(input, output) {
    dataset <- reactive({
        if (input$products=="Paper and Paperboard")
            dtf <- paperproducts$entity
        if (input$products=="Sawnwood")
            dtf <- sawnwood$entity
        if (input$products=="Roundwood")
            dtf <- roundwood$entity
        if (input$products=="Wood Panels")
            dtf <- woodpanels$entity
        dtf <- dtf %>% filter(region %in% input$region)
        # Sort countries by highest Total consumption
        highcons <- dtf[grepl("Total", dtf$item) &
                            dtf$year==max(dtf$year),]
        highcons <- highcons[order(-highcons$consumption),]
        dtf %>% filter(country %in% head(highcons$country,
                                         n=input$nbcountries))
    })

    output$plot <- renderPlot({
        p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
        if (input$color != 'None')
            p <- p + aes_string(color = input$color)
        facets <- paste(input$facet_row, '~', input$facet_col)
        if (facets != '. ~ .')
            p <- p + facet_grid(facets, scales=input$scales)
        if (input$smooth)
            p <- p + geom_smooth()
        print(p)
    }, height=700)
})



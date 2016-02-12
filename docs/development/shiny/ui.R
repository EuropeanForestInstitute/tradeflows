# Inspired by https://gist.github.com/jcheng5/3239667

# warning("ui.R getwd: ",getwd()) # Check the working directory

# Load a dataset to build the interface dynamically based on
# columns in that dataset

# Hack to run the app when the working directory is at the root
# of a project folder
# Delete if not used a couple of months after Mai 2015
# if (grepl("docs/1_explore/shiny",getwd())) setwd("../../..")

library(tradeflows)
dataset <- readdbproduct(440799, "validated_flow_yearly")


shinyUI(fluidPage(
    headerPanel("Forest Products Explorer"),
    sidebarPanel(
        selectInput('products', 'Products',
                    c('Paper and Paperboard', 'Sawnwood',
                      'Wood Panels', 'Roundwood')),
        selectInput('region', 'Regions',
                    choices = unique(dataset$region),
                    selected = "Europe",
                    multiple=TRUE, selectize=TRUE),
        sliderInput('nbcountries', 'Number of Countries',
                    min=1, max=length(unique(dataset$country)),
                    value=5, step=1, round=0),
        selectInput('x', 'X', names(dataset), selected = 'year'),
        selectInput('y', 'Y', names(dataset), selected = "consumption"),
        selectInput('color', 'Color', c('None', names(dataset)),
                    selected = NULL),
        checkboxInput('smooth', 'Smooth'),
        selectInput('facet_row', 'Facet Row', c(None='.', names(dataset)),
                    selected="item"),
        selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)),
                    selected = "country"),
        selectInput('scales', 'Scales', c("fixed", "free_y", "free_x", "free")),
        doc <- tags$html(
            h1('Metadata'),
            p('Consumption in', HTML("<strong>M<sup>3</sup></strong>"),
              'for Sawnwood, Roundwood and Wood Panels'),
            p('Consumption in', strong('T'),'for Paper and Paperboard'),
            p('GDP in constant USD of 2010'),
            p('prices in constant USD of 2010')
        )
        #         cat(as.character(doc),"bli")
        #         cat(as.character(h1('My first heading')))
    ),
    mainPanel(
        plotOutput('plot')
    )
))

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(forecast)
library(xts)
library(plotly)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Commodity Model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("commoditySelection",
                        label = "Select the commodity",
                        choices = c("Gold", "Silver", "Wheat", "Platinum"),
                        selected = "Wheat"),
            dateRangeInput("dateSelector",
                           label = "Range",
                           start = now() - dyears(20),
                           end = now()),
            numericInput("frequency",
                         label = "Frequency",
                         value = "250",
                         min = "100",
                         max = "2000"
                         ),
            actionButton(inputId = "analyseButton",
                         label = "Run Analysis")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
               column(12,
                      plotlyOutput("stlPlot", height = "500"))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    runAnalysis <- eventReactive(input$analyseButton,{
        
        req(input$frequency)
        req(input$commoditySelection)
        
        selectedCommodity <- input$commoditySelection
        
        # if(selectedCommodity == "Gold") {
        #     selection <- "AU"
        # } else if (selectedCommodity == "Silver") {
        #     selection <- "AG"
        # } else if (selectedCommodity == "Wheat"){
        #     selection <- "ZW=F"
        # } else {
        #     return()
        # }
        
        selection <- switch(selectedCommodity,
                            "Gold"   = "AU",
                            "Silver" = "AG",
                            "Platinum" = "PL=F",
                            "Wheat"  = "ZW=F")
        
        if (is.null(selection)) return()
        
        dateRanges <- input$dateSelector
        startDate <- as.character(dateRanges[1])
        endDate <- as.character(dateRanges[2])
        
        data <- get(getSymbols(selection, from = startDate, to = endDate))
        
        data <- na.approx(data)
        
        
        dataMonthly <- to.monthly(data)
        adj <- Ad(data)
        freq <- input$frequency
        
        adj.ts <- ts(adj, frequency = freq)
        
        whole.periods <- floor(nrow(adj.ts) / freq)
        partial.periods <- nrow(adj.ts) %% freq
        
        desired.test <- 3
        training.end.row <- whole.periods + 1
        training.end.col <- ifelse(partial.periods == 0, freq - desired.test, freq - partial.periods - desired.test)
        if(partial.periods < desired.test){
            training.end.row <- whole.periods
            training.end.col <- freq - (desired.test - partial.periods)
        }
        training.ts <- window(adj.ts, c(1,1), c(training.end.row,training.end.col))
        #testing.ts <- window(adj.ts, c(training.end.row, training.end.col + 1))
        
        fit.stl <- stl(training.ts[,1], s.window = "period")
        
        finalPlot <- ggplot2::autoplot(fit.stl, main = "STL Decomposition") %>% 
            plotly::ggplotly()
        
        return(finalPlot)
        
    })
    
    output$stlPlot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        stlPlot <- runAnalysis()
        
        stlPlot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

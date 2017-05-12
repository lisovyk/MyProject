library(packrat)
library(plotly)
library(shiny)

ui <- fluidPage(
    sliderInput("xy", "how big are our X,Y?", 100, min = 1, max = 1000),
    plotlyOutput("plot"),
    tableOutput('values')
)


server <- function(input, output) {
    plotvalues <- reactive({
        data.frame(
            Name = c("xy"),
            Value = as.character(input$xy),
            stringsAsFactors = FALSE
        )
    })
    
    output$plot <- renderPlotly({
        plot_ly (
            x = rnorm(input$xy, 2),
            y = rnorm(input$xy, 5),
            type = "scatter" ,
            mode = "markers" ,
            sizes = c(2, 1)
        )
    })
    output$values <- renderTable({
        plotvalues()
    })
}

shinyApp(ui = ui, server = server)

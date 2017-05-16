library(packrat)
library(plotly)
library(shiny)
library(shinymaterial)

ui <- material_page(
    title = "My shiny Shiny app, yay!",

    material_tabs(
        tabs = c(
            "Left tab" = "first_tab",
            "Right tab" = "second_tab"
        )
    ),
    material_tab_content(
        tab_id = "first_tab",
        material_slider(
            input_id = "xy",
            label = "What about xy?",
            min_value = 1,
            max_value = 300,
            initial_value = 100,
            color = "ef5350"
        ),
        plotlyOutput("plot"),
        tableOutput('values')
    ),
    material_tab_content(
        tab_id = "second_tab",
        tags$h2("Another content lies here.")
    )
)


server <- function(input, output) {
    ValueX = rnorm(300,2)
    ValueY = rnorm(300,5)
    plotvalues <- reactive({
        data.frame(
            Name = c("xy"),
            Value = as.character(input$xy),
            
            stringsAsFactors = FALSE
        )
    })
    
    output$plot <- renderPlotly({
        plot_ly (
            x = ValueX[1:input$xy],
            y = ValueY[1:input$xy],
            type = "scatter" ,
            mode = "markers" ,
            sizes = c(2, 1)
        )
    })
    output$values <- renderTable({
        c(plotvalues(), ValueX, ValueY)
    })
}

shinyApp(ui = ui, server = server)

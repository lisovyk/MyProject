library(packrat)
library(plotly)
library(shiny)
library(shinymaterial)
library(DT)

ui <- material_page(
    # tags$head(
    #     tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
    # ),
    title = "My shiny Shiny app, yay",
    
    material_side_nav(
        fixed = FALSE,
        material_card(
            title = "Shiny",
            depth = 0,
            fileInput(
                'uploaded_file',
                'Choose CSV File',
                accept = c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv'),
                buttonLabel = "Upload"
            ),
            helpText(
                "Note: 5 MB is the maximum file size...",
                "for now."
            ),
            material_checkbox(
                input_id = "header",
                label = "Header",
                initial_value = TRUE
            ),
            material_radio_button(
                input_id = "upload_sep",
                "Separator:",
                choices = c(
                    "Comma" = ",",
                    "Semicolon" = ";",
                    "Tab" = "\t"
                )
            ),
            material_radio_button(
                input_id = "upload_quote",
                "Quote:",
                choices = list(
                    "None" = "",
                    "Double Quote" = '"',
                    "Single Quote" = "'"
                )
            )
        )
    ),
    material_tabs(
        tabs = c(
            "Upload tab" = "first_tab",
            "Graphs tab" = "second_tab"
        )
    ),
    material_tab_content(
        tab_id = "first_tab",
        material_card(
            title = "User data table output:",
            DT::dataTableOutput("contents")
        )
    ),
    material_tab_content(
        tab_id = "second_tab",
        tags$br(),
        tags$h5(" some content")
    )
)
    
    
    
server <- function(input, output) {
    user_table <- reactive({
        validate(
            need(!is.null(input$uploaded_file), message = FALSE),
            errorClass = "csverr"
        )
        read.csv(
            input$uploaded_file$datapath,
            header=input$header,
            sep=input$upload_sep, 
            quote=input$upload_quote)
    })
    output$contents <- DT::renderDataTable(
        print(str(user_table())),
        datatable(user_table(), 
        selection = list(target = 'column'),
        options = list(autoWidth = FALSE, 
                       align = 'center', 
                       sDom = '<"top">rt<"bottom">ip', 
                       scrollX = TRUE, 
                       info = TRUE, 
                       paging = TRUE,
                       oLanguage = list("sZeroRecords" ="", "sEmptyTable" = ""),
                       ordering=T, 
                       pageLength = 10
                       ),
        filter = "top"
    ))
}

shinyApp(ui = ui, server = server)
    
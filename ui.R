material_page(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
    ),
    title = "My shiny Shiny app, yay",
    
    material_side_nav(
        fixed = TRUE,
        material_card(
            title = "File Upload",
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
                    "None" = " ",
                    "Double Quote" = '"',
                    "Single Quote" = "'"
                )
            )
        ),
        hr(),
        uiOutput("render_button")

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
        material_card(
            title = "User data table output:",
            DT::dataTableOutput("secondtable")
        )
    )
)
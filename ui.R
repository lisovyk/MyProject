material_page(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
    ),
    title = "My shiny Shiny app, yay",
    material_side_nav(
        fixed = TRUE,
        conditionalPanel(
            condition = "$('a.active').attr('href') == '#first_tab'",
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
                br(),
                material_switch(
                    input_id = "header_switch",
                    label = "",
                    off_label = "Header:",
                    on_label = "",
                    initial_value = TRUE,
                    color = "#42A5F5"
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
            conditionalPanel(
                condition = "output.fileUploadedBool",
                uiOutput("render_button")
            )
        ),
        conditionalPanel(
            condition = "$('a.active').attr('href') == '#second_tab'",
            uiOutput("graph_buttons")
        ),
        conditionalPanel(
            condition = "$('a.active').attr('href') == '#clustering_tab'",
            uiOutput("cluster_buttons"),
            uiOutput("button_cluster_type")
        ),
        conditionalPanel(
            condition = "$('a.active').attr('href') == '#pca_tab'",
            uiOutput("pca_buttons")
        ),
        conditionalPanel(
            condition = "$('a.active').attr('href') == '#classification_tab'",
            uiOutput("classification_buttons"),
            uiOutput("button_classificator_type")
        )
    ),
    material_tabs(
        tabs = c(
            "Upload" = "first_tab",
            "Graphs" = "second_tab",
            "Clustering" = "clustering_tab",
            "PCA" = "pca_tab",
            "Classification" = "classification_tab"
        )
    ),
    material_tab_content(
        tab_id = "first_tab",
        material_card(
            title = "User data table output:",
            DT::dataTableOutput("main_user_table")
        )
    ),
    material_tab_content(
        tab_id = "second_tab",
        material_row(
            plotlyOutput("plotlyGraph")
        )
    ),
    material_tab_content(
        tab_id = "clustering_tab",
        uiOutput("clustTab")
        
    ),
    material_tab_content(
        tab_id = "pca_tab",
        plotlyOutput("plotlyPCA"),
        plotlyOutput("pca_explained"),
        dataTableOutput("PCtable")
    ),
    material_tab_content(
        tab_id = "classification_tab",
        h5("Train set predicions"),
        DT::dataTableOutput("confusion_matrix2"),
        tableOutput("confMatAcc2"),
        h5("Test set predictions"),
        DT::dataTableOutput("confusion_matrix1"),
        tableOutput("confMatAcc1"),
        h5("Variance Importance"),
        tableOutput("varImportance"),
        plotOutput("cuttree")
    )
)

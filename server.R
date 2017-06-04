function(input, output) {
    rv <- reactiveValues()
    user_table <- reactive({
        validate(
            need(!is.null(input$uploaded_file), message = FALSE),
            errorClass = "csverr"
        )
        fread(
            input$uploaded_file$datapath,
            header = input$header_switch,
            sep = input$upload_sep,
            quote = input$upload_quote,
            stringsAsFactors = FALSE
        )
    })
    
    observeEvent(input$uploaded_file, {
        rv$user_table_init <- user_table()
        rv$userTable <- user_table()
    })
    #functions
    convert.types <- function(obj, types){
        for(i in 1:nrow(types)){
            if( class(obj[,i]) != types[i, 1]) {
                func <- switch(types[i, 1],
                               integer = as.integer,
                               numeric = as.numeric,
                               factor = as.factor,
                               character = as.character,
                               logical = as.logical)
                obj[,i] <- func(obj[,i])
            }
        }
        obj
    }

    #buttons ui
    button_render <- renderUI({
        if(!is.null(input$uploaded_file))
            material_card(
                actionButton(
                    inputId = "button_table_convertion",
                    label = "Apply"
                ),
                material_checkbox(
                    input_id = "remove_na",
                    label = "Remove NA's",
                    initial_value = TRUE
                ),
                material_checkbox(
                    input_id = "remove_col",
                    label = "Remove columns",
                    initial_value = FALSE
                ),
                material_checkbox(
                    input_id = "checkbox_delete_rows",
                    label = "Delete selected rows",
                    initial_value = TRUE
                ),
                rHandsontableOutput("handsontypes")
            )
    })
    button_graph <- renderUI({
        if(!is.null(input$uploaded_file)){
            material_row(
                material_column(
                    width = 2,
                    actionButton(
                        inputId = "graphButton",
                        label = "Make graph",
                        depth = 0
                    )
                ),
                material_column(
                    width = 2,
                    selectInput(
                        inputId = "graph_x",
                        label = "X axis",
                        choices = rv$AvailableCols,
                        multiple = FALSE
                    ),
                    selectInput(
                        inputId = "graph_y",
                        label = "Y axis",
                        choices = rv$AvailableCols,
                        selected = c("c"),
                        multiple = FALSE
                    )
                ),
                material_column(
                    width = 2,
                    selectInput(
                        inputId = "plotlyColor",
                        label = "Coloring",
                        choices = c("Standart", colnames(rv$userTable)),
                        selected = c("Standart"),
                        multiple = FALSE
                    )
                )
            )
        }
    })
    

    #buttons events
    observeEvent(c(input$button_table_convertion), {
        if(input$button_table_convertion >= 1){
            if(!input$checkbox_delete_rows){
                rv$user_table_init <- user_table()
            }
            rv$userTable <- as.data.frame(rv$user_table_init, stringsAsFactors = FALSE)
                if(input$checkbox_delete_rows & !is.null(input$main_user_table_rows_selected) ) {
                    rv$user_table_init <- rv$userTable[-as.numeric(input$main_user_table_rows_selected), ]
                    rv$userTable <- rv$user_table_init
                }
                user_input <- hot_to_r(input$handsontypes)
                rv$userTable <- convert.types(rv$userTable, user_input)
                
                if(input$remove_na) {
                    rv$userTable <- rv$userTable[complete.cases(rv$userTable),]
                }
                
                if(input$remove_col) {
                    drops <- vector(mode = "character")
                    for(i in 1:nrow(user_input)) {
                        if(user_input[i, "UseColumn"] == FALSE) { 
                            drops <- c(rownames(user_input[i, ]), drops)
                        }
                    }
                    user_input <- user_input[!(rownames(user_input) %in% drops), ]
                    rv$userTable <- rv$userTable[ , rownames(user_input)]
                }
        }
    })
    
    observeEvent(c(input$graphButton), {
        if(input$graphButton >= 1) {
            rv$ColnameX <- input$graph_x
            rv$ColnameY <- input$graph_y
            rv$graphColor <- NULL
            rv$plotlyType <- "scatter"
            if(input$plotlyColor != "Standart"){
                if (class(rv$userTable[, input$plotlyColor]) == "integer" ||
                    class(rv$userTable[, input$plotlyColor]) == "numeric" ) {
                    rv$graphColor <- rv$userTable[, input$plotlyColor]
                } else if (class(rv$userTable[, input$plotlyColor]) == "factor")
                    rv$graphColor <- rv$userTable[, input$plotlyColor]
            }
        }       
    })
    
    #Available names for Axises
    observeEvent(rv$userTable, {
        rv$AvailableCols <- character(0)
        ColClass <- sapply(rv$userTable, class)
        for(i in 1:length(ColClass)){
            if(ColClass[i] == "numeric" || ColClass[i] == "factor" || ColClass[i] == "integer"){
                rv$AvailableCols <- c(rv$AvailableCols, names(ColClass[i]))
            }
            rv$AvailableCols
        }
    })
    
    
    typesDF <- reactive({
        DF1 = data.frame(Type = cbind(lapply(rv$userTable, class)),
                        UseColumn = rep(TRUE, times = length(rv$userTable)),
                        NAs = sapply(rv$userTable, function(y) sum(is.na(y))),
                        stringsAsFactors = FALSE)
        DF1$Type = factor(DF1$Type, dataTypes)
        DF1$UseColumn = factor(DF1$UseColumn, c(TRUE, FALSE))
        DF2 = data.frame(Type = cbind(lapply(rv$user_table_init, class)),
                         UseColumn = rep(TRUE, times = length(rv$user_table_init)),
                         NAs = sapply(rv$user_table_init, function(y) sum(is.na(y))),
                         stringsAsFactors = FALSE)
        DF2$Type = factor(DF2$Type, dataTypes)
        DF2$UseColumn = factor(DF2$UseColumn, c(TRUE, FALSE))
        merge(DF2,DF1, all.x = TRUE)
    })
    dataTypes <- c("integer", "numeric", "factor", "character", "logical")
    types_table <- renderRHandsontable({
        DF = data.frame(Type = cbind(lapply(rv$userTable, class)),
                        UseColumn = rep(TRUE, times = length(rv$userTable)),
                        NAs = sapply(rv$userTable, function(y) sum(is.na(y))),
                        stringsAsFactors = FALSE)
        DF$Type = factor(DF$Type, dataTypes)
        DF$UseColumn = factor(DF$UseColumn, c(TRUE, FALSE))
        rhandsontable(DF, selectCallback = TRUE, readOnly = FALSE) %>%
            hot_table(stretchH = 'all') %>%
            hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    })
    
    
    main_user_table <- DT::renderDataTable({
        validate(
            need(is.data.frame(rv$userTable), message = FALSE),
            errorClass = "main_table data"
        )
        datatable(rv$userTable,
                  selection = list(target = 'row'),
                  options = list(
                      autoWidth = FALSE,
                      align = 'center',
                      sDom = '<"top">rt<"bottom">ip',
                      scrollX = TRUE,
                      info = TRUE,
                      paging = TRUE,
                      oLanguage = list("sZeroRecords" = "", "sEmptyTable" = ""),
                      ordering = T,
                      pageLength = 10
                  ),
                  filter = "top",
                  colnames = paste0(colnames(rv$userTable),
                                    " (",
                                    cbind(lapply(rv$userTable, class)),
                                    ")")
        )
    })
    
    plotlygraph <- renderPlotly({
        validate(
            need(input$graphButton >= 1, message = FALSE),
            errorClass = "plotly_err"
        )
        plot_ly(rv$userTable,
                type = rv$plotlyType,
                x = rv$userTable[, rv$ColnameX],
                y = rv$userTable[, rv$ColnameY],
                color = rv$graphColor)

        
    })

   
    
    output$main_user_table <- main_user_table
    output$handsontypes <- types_table
    output$render_button <- button_render
    output$graph_buttons <- button_graph
    output$plotlyGraph <- plotlygraph
    
}

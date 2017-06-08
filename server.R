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
            if(class(obj[,i]) != types[i, 1]) {
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
                rHandsontableOutput("handsontypes"),
                uiOutput("text_caution")
            )
    })
    button_graph <- renderUI({
        if(!is.null(input$uploaded_file)){
            material_row(
                actionButton(
                    inputId = "graphButton",
                    label = "Make graph",
                    depth = 0
                ),
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
                ),
                selectInput(
                    inputId = "plotlyColor",
                    label = "Coloring",
                    choices = rv$AvailableColoring,
                    selected = c("Standart"),
                    multiple = FALSE
                )
            )
        }
    })
    button_cluster <- renderUI({
        if(!is.null(input$uploaded_file)) {
            material_card(
                material_row(
                    tags$div( 
                        title = "In order to perform clustering, there should be no NA values in your data frame",
                        actionButton(
                            inputId = "clusterButton",
                            label = "Perform"
                        )
                    ),
                    selectInput(
                        inputId = "cluster_alg",
                        label = "Clustering algorithm",
                        choices = c("asd","dsa"),
                        multiple = FALSE
                    ),
                    sliderInput(
                        inputId = "cluster_itermax",
                        label = "Max iterations",
                        min = 1,
                        max = 1000,
                        value = 100
                    ),
                    sliderInput(
                        inputId = "cluster_nstart",
                        label = "Nstart",
                        min = 1,
                        max = 100,
                        value = 10
                    ),
                    numericInput(
                        inputId = "cluster_number",
                        label = "N clusters",
                        min = 2,
                        max = nrow(rv$userTable), #bugged: user can manually write a number > max
                        value = 2
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
    
    observeEvent(c(input$clusterButton), {
        if(input$clusterButton >= 1) {
            rv$clusterTable <- rv$userTable[sapply(rv$userTable, is.numeric)]
            rv$tableCluster <- kmeans(rv$clusterTable,
                                      centers = input$cluster_number,
                                      nstart = input$cluster_nstart,
                                      iter.max = input$cluster_itermax)
            rv$tableCluster$cluster <- as.factor(rv$tableCluster$cluster)
            
            rv$clusterBar <- data.frame(1:length(rv$tableCluster$size))
            for(i in 1:length(rv$tableCluster$size)) {
                rv$clusterBar[i,2] <- rv$tableCluster$size[i]
            }
            colnames(rv$clusterBar) <- c("Cluster no.", "Cluster size")
        }
    })
    
    #Available names and colors on dropdowns
    observeEvent(rv$userTable, {
        rv$AvailableCols <- character(0)
        rv$AvailableColoring <- character(0)
        ColClass <- sapply(rv$userTable, class)
        for(i in 1:length(ColClass)) {
            if(ColClass[i] == "numeric" || ColClass[i] == "integer"){
                rv$AvailableCols <- c(rv$AvailableCols, names(ColClass[i]))
            }
            if(ColClass[i] != "character") {
                rv$AvailableColoring <- c(rv$AvailableColoring, names(ColClass[i]))
            }
        }

    })
    
    dataTypes <- c("integer", "numeric", "factor", "character", "logical")
    types_table <- renderRHandsontable({
        DF = data.frame(Type = cbind(lapply(rv$userTable, class)),
                        UseColumn = rep(TRUE, times = length(rv$userTable)),
                        NAs = sapply(rv$userTable, function(y) sum(is.na(y))),
                        stringsAsFactors = FALSE)
        if(!is.null(input$handsontypes)) {
            DF <- merge(hot_to_r(input$handsontypes),
                        DF,
                        by.x = "Type",
                        by.y = 0,
                        all.x = TRUE,
                        suffixes = c("", ""))[,1:ncol(DF)]
            rownames(DF) <- rownames(hot_to_r(input$handsontypes))
            DF$Type <- hot_to_r(input$handsontypes)[,"Type"]
            DF$UseColumn <- hot_to_r(input$handsontypes)[,"UseColumn"]
            DF$NAs <- hot_to_r(input$handsontypes)[,"NAs"]
        }
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
    
    cluster_user_table <- DT::renderDataTable({
        validate(
            need(!is.null(rv$tableCluster), message = FALSE),
            errorClass = "main_table data"
        )
        dt <- rv$userTable
        dt["Cluster"] <- rv$tableCluster$cluster
        datatable(dt,
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
                  colnames = paste0(colnames(dt),
                                    " (",
                                    cbind(lapply(dt, class)),
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
                color = rv$graphColor) %>%
            layout(title = "Your graph",
                   xaxis = list(title = rv$ColnameX),
                   yaxis = list(title = rv$ColnameY)
                   )
    })
    cluster_table <- renderPlotly({
        validate(
            need(input$clusterButton >= 1, message = FALSE),
            errorClass = "cluster_err"
        )
        plot_ly(rv$clusterTable,
                type = "scatter",
                x = rv$clusterTable[, 1],
                y = rv$clusterTable[, 2],
                color = rv$tableCluster$cluster) %>%
            layout(title = "Cluster graph")
    })
    cluster_barplot <- renderPlotly({
        validate(
            need(input$clusterButton >= 1, message = FALSE),
            errorClass = "cluster_barplot_err"
        )
        plot_ly(rv$clusterBar,
                type = "bar",
                x = as.factor(rv$clusterBar[,"Cluster no."]),   # no floats on x axis
                y = rv$clusterBar[,"Cluster size"],
                color = as.factor(rv$tableCluster$size)) %>%
            layout(title = "Number of items in each cluster",
                   xaxis = list(title = "Cluster no."),
                   yaxis = list(title = "Cluster size")
            )
    })

    output$text_caution <- renderUI({
        removeClass(id = "text_caution", class = "greentext")
        removeClass(id = "text_caution", class = "redtext")

        output <- paste("Everything is OK!")
        if(any(apply(rv$userTable, 2, function(x) any(is.na(x))))) {
            output <- paste("Careful! You have NA values in dataset.")
            toggleClass(id = "text_caution", class = "redtext")
        }
        if(output == paste("Everything is OK!")) {
            toggleClass(id = "text_caution", class = "greentext")
        }
        tags$i(output)
        
    })
   
    
    
    observe({
            if (!is.null(input$clusterButton)) {
                # or if ("clusterButton" %in% names(input)) 
                disable("clusterButton")
                observeEvent(rv$userTable, {
                    rv$anyNAs <- any(sapply(rv$userTable, function(y) sum(is.na(y))))
                    toggleState(id = "clusterButton", condition = !rv$anyNAs)
                })
            }
    })

    output$fileUploadedBool <- reactive({
        return(!is.null(user_table()))
    })
    outputOptions(output, 'fileUploadedBool', suspendWhenHidden = FALSE)
        

    output$cluster_buttons <- button_cluster
    output$clusterBarplot <- cluster_barplot
    output$clusterTable <- cluster_table
    output$main_user_table <- main_user_table
    output$handsontypes <- types_table
    output$render_button <- button_render
    output$graph_buttons <- button_graph
    output$plotlyGraph <- plotlygraph
    output$clusterUserTable <- cluster_user_table
}

function(input, output) {
    rv <- reactiveValues()
    user_table <- reactive({
        validate(
            need(!is.null(input$uploaded_file), message = FALSE),
            errorClass = "csverr"
        )
        fread(
            input$uploaded_file$datapath,
            header = input$header,
            sep = input$upload_sep,
            quote = ifelse(input$upload_quote == " ", "", input$upload_quote),
            stringsAsFactors = FALSE
        )
    })
    observeEvent(user_table(), {
        rv$user_table_init <- user_table()
        })
    
    #functions
    convert.types <- function(obj, types){
        for(i in 1:length(types)){
            if( typeof(obj[,i]) == types[i, 1]) { next }
            
            func <- switch(types[i, 1],
                           integer = as.integer,
                           numeric = as.numeric,
                           factor = as.factor,
                           character = as.character,
                           logical = as.logical,
                           double = as.double)
            obj[,i] <- func(obj[,i])
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
                    initial_value = FALSE
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
    

    #buttons events
    observeEvent(c(input$button_table_convertion), {
        if(!input$checkbox_delete_rows){
            rv$user_table_init <- user_table()
        }
        rv$userTable <- as.data.frame(rv$user_table_init, stringsAsFactors = FALSE)
        if(input$button_table_convertion) {  #  doesnt work w/o it! error on hot_to_r(input$handsontypes)
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
    
    
    dataTypes <- c("integer", "numeric", "factor", "character", "logical", "double")
    types_table <- renderRHandsontable({
        DF = data.frame(Type = cbind(lapply(rv$userTable, typeof)),
                        UseColumn = rep(TRUE, times = length(rv$userTable)),
                        NAs = sapply(rv$userTable, function(y) sum(is.na(y))),
                        stringsAsFactors = FALSE)
        DF$Type = factor(DF$Type, dataTypes)
        DF$UseColumn = factor(DF$UseColumn, c(TRUE, FALSE))
        rhandsontable(DF, selectCallback = TRUE, readOnly = FALSE) %>%
            # hot_table(stretchH = 'all') %>%
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
                                    cbind(lapply(rv$userTable, typeof)),
                                    ")")
        )
    })

    
    output$main_user_table <- main_user_table
    output$handsontypes <- types_table
    output$render_button <- button_render
}

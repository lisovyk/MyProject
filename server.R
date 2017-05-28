function(input, output) {
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

    
    #buttons
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
    
    
    dataTypes <- c("integer", "numeric", "factor", "character", "logical", "double")
    types_table <- renderRHandsontable({
        if (!is.null(input$uploaded_file)) {
            if (is.null(input$handsontypes)) {
                DF = data.frame(Type = cbind(lapply(new_table(), typeof)),
                                UseColumn = rep(TRUE, times = length(new_table())),
                                NAs = sapply(rv$userTable, function(y) sum(is.na(y))),
                                stringsAsFactors = FALSE)
                DF$Type = factor(DF$Type, dataTypes)
                DF$UseColumn = factor(DF$UseColumn, c(TRUE, FALSE))
            } else {
                DF = hot_to_r(input$handsontypes)
            }
            rhandsontable(DF, selectCallback = TRUE, readOnly = FALSE) %>%
                # hot_table(stretchH = 'all') %>%
                hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        }
        else { return() }
    })
    rhandsontab <- eventReactive(rv$userTable, {
        
    })

    observe({
        print(sapply(rv$userTable, function(y) sum(is.na(y))))
        })
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
    
    
    rv <- reactiveValues(
        userTable = NULL,
        types_table = NULL
    )
    new_table <- eventReactive(c(input$button_table_convertion), {
        if(!is.null(input$uploaded_file)) {
            print(str(rv$userTable))
            if(is.null(rv$userTable)) {
                rv$userTable <- data.frame(user_table(), stringsAsFactors = FALSE)
                colnames(rv$userTable) <- colnames(user_table())
            }
            if(input$button_table_convertion) {
                hottestVector <- hot_to_r(input$handsontypes)
                rv$userTable <- convert.types(rv$userTable, hottestVector)
                
                if(input$checkbox_delete_rows & !is.null(input$main_user_table_rows_selected)) {
                    rv$userTable <- rv$userTable[-as.numeric(input$main_user_table_rows_selected), ]
                }
                # if(!input$checkbox_delete_rows){
                #     rv$userTable <- 
                # }
                
                if(input$remove_na) {
                    hottestVector <- hot_to_r(input$handsontypes)
                    rv$userTable <- convert.types(rv$userTable, hottestVector)
                    rv$userTable <- rv$userTable[complete.cases(rv$userTable),]
                }
                
                if(input$remove_col) {
                    drops <- vector(mode = "character")
                    hottestVector <- hot_to_r(input$handsontypes)
                    for(i in 1:nrow(hottestVector)) {
                        if(hottestVector[i, "UseColumn"] == FALSE) { 
                            drops <- c(rownames(hottestVector[i, ]), drops)
                        }
                    }
                    hottestVector <- hottestVector[!(rownames(hottestVector) %in% drops), ]
                    rv$userTable <- rv$userTable[ , rownames(hottestVector)]
                }
            }

        }
        
        rv$userTable
    })
    # observeEvent(input$button_delete_rows, {
    #     if(!is.null(input$button_delete_rows)) {
    #         userTable <- data.frame(user_table(), stringsAsFactors = FALSE)
    #         colnames(userTable) <- colnames(user_table())
    #         userTable <- userTable[-as.numeric(input$main_user_table_rows_selected), ]
    #     }
    # })

    # main_table <- DT::renderDataTable({
    #     datatable(
    #         user_table(),
    #         colnames = paste0(colnames(user_table()),
    #                           " (",
    #                           cbind(lapply(user_table(), typeof)),
    #                           ")"),
    #         options = list(
    #             autoWidth = FALSE,
    #             align = 'center',
    #             sDom = '<"top">rt<"bottom">ip',
    #             scrollX = TRUE,
    #             info = TRUE,
    #             paging = TRUE,
    #             oLanguage = list("sZeroRecords" = "", "sEmptyTable" = ""),
    #             ordering = T,
    #             pageLength = 10
    #         ),
    #         filter = "top"
    #     )
    # })

    main_user_table <- DT::renderDataTable({
        validate(
            need(is.data.frame(new_table()), message = FALSE),
            errorClass = "main_table data"
        )
        datatable(new_table(),
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
                  colnames = paste0(colnames(new_table()),
                                    " (",
                                    cbind(lapply(new_table(), typeof)),
                                    ")")
        )
    })

    output$main_user_table <- main_user_table
    output$handsontypes <- types_table
    output$render_button <- button_render
}
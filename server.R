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
    
    main_table <- DT::renderDataTable({
        datatable(
            user_table(),
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
            filter = "top"
        )
    })
    
    
    button_render <- renderUI({
        if(!is.null(input$uploaded_file))
            material_card(
                actionButton(
                    inputId = "button_table_convertion",
                    label = "Convert"
                ),
                rHandsontableOutput("handsontypes")
            )
    })
    dataTypes <- c("integer", "numeric", "factor", "character", "logical")
    types_table <- renderRHandsontable({
        if (!is.null(input$uploaded_file)) {
            if (is.null(input$handsontypes)) {
                DF = data.frame(Type = cbind(lapply(user_table(), typeof)),
                                stringsAsFactors = TRUE)
                DF$Type = factor(DF$Type, dataTypes)
            } else {
                DF = hot_to_r(input$handsontypes)
            }
            rhandsontable(DF, readOnly = FALSE) %>%
                hot_table(stretchH = 'all', rowHeaderWidth = 120) %>%
                hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        }
        else { return() }
    })

    convert.types <- function(obj, types){
        for(i in 1:length(types)){
            # if (types[i] == 'integer')
            #     obj[,i] <- as.integer(obj[,i])
            func <- switch(types[i],
                           integer = as.integer,
                           numeric = as.numeric,
                           factor = as.factor,
                           character = as.character,
                           logical = as.logical)
            obj[,i] <- func(obj[,i])
        }
        print(str(obj))
        obj
    }

    observe({
        if (!is.null(input$handsontypes))
            print(as.vector(t(hot_to_r(input$handsontypes))))
    })
    
    new_table <- eventReactive(input$button_table_convertion, {
        hottestVector <- as.vector(t(hot_to_r(input$handsontypes)))
        convert.types(data.frame(user_table()), hottestVector)
    })
    
    
    observeEvent(input$button_table_convertion, {
        print(str(new_table()))
        print(hot_to_r(input$handsontypes))
        })
    
    output$secondtable <- DT::renderDataTable({ datatable(new_table()) })
    # output$buttclick <- button_click
    output$handsontypes <- types_table
    output$render_button <- button_render
    output$contents <- main_table
}
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
            selection = list(target = 'column'),
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
                material_button(
                    input_id = "button_table_convertion",
                    label = "Convert"
                )
            )
    })
    dataTypes <- c("integer", "numeric", "factor", "character", "logical")
    handsontable <- renderRHandsontable({
        if (!is.null(input$uploaded_file)) {
            if (is.null(input$hottest)) {
                DF = data.frame(Type = cbind(lapply(user_table(), typeof)),
                                stringsAsFactors = TRUE)
            } else {
                DF = hot_to_r(input$hottest)
            }
            DF$Type = factor(DF$Type, dataTypes)
            rhandsontable(DF, readOnly = FALSE) %>%
                hot_table(stretchH = 'all', rowHeaderWidth = 120) %>%
                hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        }
        else { return() }
    })
    output$hottest <- handsontable
    output$render_button <- button_render
    output$contents <- main_table
}
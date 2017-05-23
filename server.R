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
    
    editable_table <- DT::renderDataTable({
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
    
    edit_types <- DT::renderDataTable({
        tab <- cbind(lapply(user_table(), typeof))
        colnames(tab) <- "Type"
        datatable(
            tab,
            selection = list(target = 'column'),
            options = list(
                dom = 't',
                autoWidth = FALSE,
                align = 'center',
                scrollX = FALSE,
                info = TRUE,
                searching = FALSE,
                paging = FALSE,
                ordering = FALSE
            ),
            filter = "none"
        )
    })
    
    button_render <- renderUI({
        if(!is.null(input$uploaded_file))
            material_card(
                DT::dataTableOutput("datatypes"),
                material_button(
                    input_id = "button_table_convertion",
                    label = "Convert"
                )
            )
    })
    dataTypes <- c("integer", "numeric", "factor", "character", "logical")
    output$hottest <- renderRHandsontable({
        if (!is.null(input$uploaded_file)) {
            if (is.null(input$hottest)) {
                DF = data.frame(Type = cbind(lapply(user_table(), typeof)),
                                stringsAsFactors = FALSE)
            } else {
                DF = hot_to_r(input$hottest)
            }
            rhandsontable(DF,
                          readOnly = FALSE) %>%
                hot_col(col = "Type", type = "dropdown", source = dataTypes, readOnly = FALSE) %>%
                hot_table(stretchH = 'all', rowHeaderWidth = 120) %>%
                hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
            
        }
        else { return() }
        
        
    })
    output$render_button <- button_render
    output$datatypes <- edit_types
    output$contents <- editable_table
}

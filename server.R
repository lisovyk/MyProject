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
        # rhandsontable(tab)
    })
    # output$hot <- renderRHandsontable({
    #     if (is.null(input$hot)) {
    #         DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
    #                         dt = seq(from = Sys.Date(), by = "days", length.out = 10),
    #                         stringsAsFactors = F)
    #     } else {
    #         DF = hot_to_r(input$hot)
    #     }
    #     rhandsontable(DF, useTypes = as.logical(input$useType)) %>%
    #         # hot_table(readOnly = TRUE) # ok
    #         hot_col(col = 1, readOnly = FALSE) 
    # })
    
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
    output$render_button <- button_render
    output$datatypes <- edit_types
    output$contents <- editable_table
}

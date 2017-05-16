function(input, output) {
    user_table <- reactive({
        validate(
            need(!is.null(input$uploaded_file), message = FALSE),
            errorClass = "csverr"
        )
        read.csv(
            input$uploaded_file$datapath,
            header=input$header,
            sep=input$upload_sep, 
            quote=input$upload_quote,
            stringsAsFactors = FALSE
            )
    })

    output$contents <- DT::renderDataTable({
        print(str(user_table()))
        
        datatable(user_table(), 
                  selection = list(target = 'column'),
                  options = list(autoWidth = FALSE, 
                                 align = 'center', 
                                 sDom = '<"top">rt<"bottom">ip', 
                                 scrollX = TRUE, 
                                 info = TRUE, 
                                 paging = TRUE,
                                 oLanguage = list("sZeroRecords" ="", "sEmptyTable" = ""),
                                 ordering=T, 
                                 pageLength = 10
                  ),
                  filter = "top"
        )
    })
}
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
            colnames = paste(colnames(user_table()),
                             paste0(paste0("(", cbind(lapply(user_table(), typeof)), ")")),
                             sep = " "),
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
    
    #buttons
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
    
    button_delete_row <- renderUI({
        if(!is.null(new_table()))
            print(str(new_table()))
    })
    
    dataTypes <- c("integer", "numeric", "factor", "character", "logical", "double")
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
            if( typeof(obj[,i]) == types[i]) { next }
                
            func <- switch(types[i],
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
    
    new_table <- eventReactive(c(input$button_table_convertion, input$remove_na), {
        hottestVector <- as.vector(t(hot_to_r(input$handsontypes)))
        dt <- convert.types(data.frame(user_table()), hottestVector)
        print(input$remove_na)
        if(input$remove_na) {
            dt <- dt[complete.cases(dt),]
        }
        dt
    })

    second_table <- DT::renderDataTable({
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
                  colnames = paste(colnames(new_table()),
                                   paste0(paste0("(", cbind(lapply(new_table(), typeof)), ")")),
                                   sep = " ")
        )
    })
    
    # remove_nas <- eventReactive(input$second_table_remove_na, {
    #     data.frame(new_table()[complete.cases(new_table()), ])
    # })
    # third_table <- DT::renderDataTable({
    #     remove_nas()
    # })
    
    
    # output$thirdtable <- third_table
    output$secondtable <- second_table
    output$handsontypes <- types_table
    output$render_button <- button_render
    output$contents <- main_table
}
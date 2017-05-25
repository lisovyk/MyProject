function(input, output, session) {
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
                rHandsontableOutput("hottest")
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
            rhandsontable(DF, readOnly = FALSE, selectCallback = TRUE) %>%
                hot_table(stretchH = 'all', rowHeaderWidth = 120) %>%
                hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
        }
        else { return() }
    })
    
    # editable_table <-  DT::renderDataTable ({               
    #     datatable(
    #         user_table(),
    #         selection = list(target = 'column'),
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

    convert.types <- function(obj, types){
        for(i in length(obj)){
            func <- switch(types[i],
                           integer = as.integer,
                           numeric = as.numeric,
                           factor = as.factor,
                           character = as.character,
                           logical = as.logical)
            obj[,i] <- func(obj[,i])
        }
        obj
    }

        # for(r in input$hottest)
        #     print(input$hottest[r,])
        
        # for(cn in names(hot_to_r(input$hottest)))
        #     taab[[cn]] <- 
        #     
        #     print(nrow(hot_to_r(input$hottest)))
        # new_DT <- transform(user_table())
    
    observeEvent(input$button_table_convertion, {
        hottestVector <- as.vector(t(hot_to_r(input$hottest)))
        new_table <- convert.types(as.data.frame(user_table()), hottestVector)
        # print(new_table)
        str(new_table)
    }
    )
    # output$buttclick <- button_click
    output$hottest <- handsontable
    output$render_button <- button_render
    output$contents <- main_table
}
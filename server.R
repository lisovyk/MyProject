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
        rv$user_table_init <- data.frame(user_table())
        rv$userTable <- data.frame(user_table())
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
    colLab <- function(n) {
        if (is.leaf(n)) {
            a <- attributes(n)
            labCol <- dendLabelColors[rv$tableCluster$cluster[which(names(rv$tableCluster$cluster) == a$label)]]
            attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
        }
        n
    }
    dendLabelColors <- c("#e6194b", "#3cb44b", "#ffe119", "#0082c8", "#f58231",
                         "#911eb4", "#46f0f0", "#f032e6", "#d2f53c", "#fabebe",
                         "#008080", "#e6beff", "#800000", "#aaffc3", "#000080")
    
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
            material_checkbox(
                input_id = "normalize",
                label = "Apply normalization",
                initial_value = FALSE
            ),
            rHandsontableOutput("handsontypes"),
            uiOutput("text_caution")
        )
    })
    button_graph <- renderUI({
        if(!is.null(input$uploaded_file)){
            material_card(
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
            )
        }
    })
    button_cluster <- renderUI({
        if(!is.null(input$uploaded_file)) {
            material_card(
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
                    choices = c("K-means","EM", "Hierarchical"),
                    multiple = FALSE
                )
            )
        }
    })
    
    button_cluster_type <- renderUI({
        validate(
            need(length(input$cluster_alg) != 0, message = FALSE),
            errorClass = "cluster button err"
        )

        if(input$cluster_alg == "K-means") {
            material_card(
                material_row(
                    selectInput(
                        inputId = "cluster_x",
                        label = "X output",
                        choices = colnames(rv$clusterTable),
                        multiple = FALSE
                    ), 
                    selectInput(
                        inputId = "cluster_y",
                        label = "Y output",
                        choices = colnames(rv$clusterTable),
                        multiple = FALSE
                    )
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
        
        } else if (input$cluster_alg == "EM") {
            material_card(
                selectInput(
                    inputId = "cluster_x",
                    label = "X output",
                    choices = colnames(rv$clusterTable),
                    multiple = FALSE
                ),
                selectInput(
                    inputId = "cluster_y",
                    label = "Y output",
                    choices = colnames(rv$clusterTable),
                    multiple = FALSE
                ),
                numericInput(
                    inputId = "cluster_number",
                    label = "N clusters",
                    min = 2,
                    max = nrow(rv$userTable), #bugged: user can manually write a number > max
                    value = 2
                )
            )
            
        
            
        } else if(input$cluster_alg == "Hierarchical") {
            material_card(
                actionButton(
                    inputId = "clusterUpdate",
                    label = "Update"
                ),
                numericInput(
                    inputId = "hc_k",
                    label = "Number of Leaves",
                    value = 2,
                    min = 2,
                    max = 20
                ),
                selectInput(
                    inputId = "hcmetric",
                    label = "Metric",
                    choices = c("euclidean", "manhattan", "gower"),
                    selected = "euclidean"
                ),
                selectInput(
                    inputId = "hcmethod",
                    label = "Method",
                    choices = c("ward.D", "ward.D2", "single", "complete",
                                "average", "mcquitty", "median", "centroid"),
                    selected = "complete"
                )
            )
        }
    })
    
    pca_buttons <- renderUI({
        if(!is.null(input$uploaded_file)) {
            material_card(
                actionButton(
                    inputId = "pcaButton",
                    label = "Analyse"
                ),
                material_row(
                    selectInput(
                        inputId = "pca_1",
                        label = "pca 1",
                        choices = colnames(rv$pca_prep$x),
                        selected = "PC1",
                        multiple = FALSE
                    ),
                    selectInput(
                        inputId = "pca_2",
                        label = "pca 2",
                        choices = colnames(rv$pca_prep$x),
                        selected = "PC2",
                        multiple = FALSE
                    )
                )
            )        
        }
    })
    classification_buttons <- renderUI({
        if(!is.null(input$uploaded_file)) {
            material_card(
                actionButton(
                    inputId = "confMat",
                    label = "ConfMatrix"
                )
            )
        }
    })
    
    #buttons events
    observeEvent(c(input$button_table_convertion), {
        if(input$button_table_convertion >= 1){
            if(!input$checkbox_delete_rows){
                rv$user_table_init <- data.frame(user_table())
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
            if(input$normalize) {
                for(i in 1:length(colnames(rv$userTable))) {
                    if(class(rv$userTable[,i]) == "numeric" ||
                       class(rv$userTable[,i]) == "integer") {
                        rv$userTable[,i] <- as.vector(scale(rv$userTable[,i])) }
                }
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
    observeEvent(rv$userTable, {
        rv$clusterTable <- data.frame(rv$userTable[complete.cases(rv$userTable),])[sapply(rv$userTable, is.numeric)]
    })
    observeEvent(c(input$clusterButton), {
        if(input$clusterButton >= 1 & input$cluster_alg == "K-means") {
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
        
        if(input$clusterButton >= 1 & input$cluster_alg == "EM") {
            rv$tableCluster <- em.EM(rv$clusterTable,
                                     nclass = input$cluster_number)
            rv$tableCluster$cluster <- as.factor(rv$tableCluster$class)
            
            rv$clusterBar <- data.frame(1:length(rv$tableCluster$nc))
            for(i in 1:length(rv$tableCluster$nc)) {
                rv$clusterBar[i,2] <- rv$tableCluster$nc[i]
            }
            colnames(rv$clusterBar) <- c("Cluster no.", "Cluster size")
        }
        
        if(input$clusterButton >= 1 & input$cluster_alg == "Hierarchical") {
            rv$hclust <- rv$clusterTable %>%
                daisy(metric = input$hcmetric) %>%
                hclust(method = input$hcmethod)
            
        }
    })
    
    # Unavailability of button if no file uploaded
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
        DF <- data.frame(Type = cbind(lapply(rv$userTable, class)),
                         UseColumn = rep(TRUE, times = length(rv$userTable)),
                         NAs = sapply(rv$userTable, function(y) sum(is.na(y))),
                         stringsAsFactors = FALSE)
        if(!is.null(input$handsontypes)) {
            if(all(rownames(DF) == rownames(hot_to_r(input$handsontypes)))) {
                DF <- merge(data.frame(hot_to_r(input$handsontypes)),
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
    
    # Clustering
    observeEvent(c(input$clusterButton, input$clusterUpdate), { 
        if(input$cluster_alg == "EM" || input$cluster_alg == "K-means") {
            cluster_user_table <- DT::renderDataTable({
                validate(
                    need(!is.null(rv$tableCluster$cluster), message = FALSE),
                    errorClass = "main_table data"
                )
                dt <- rv$userTable[sapply(rv$userTable, is.numeric)]
                dt["Cluster"] <- as.factor(rv$tableCluster$cluster)
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
            output$clusterUserTable <- cluster_user_table
            
            cluster_table <- renderPlotly({
                validate(
                    need(input$clusterButton >= 1 & !is.null(rv$tableCluster$cluster),message = FALSE),
                    errorClass = "cluster_err"
                )
                df <- rv$clusterTable
                plot_ly(df,
                        type = "scatter",
                        x = df[, input$cluster_x],
                        y = df[, input$cluster_y],
                        color = rv$tableCluster$cluster) %>%
                    layout(title = "Cluster graph",
                           xaxis = list(title = input$cluster_x),
                           yaxis = list(title = input$cluster_y)
                    )
            })
            output$clusterTable <- cluster_table
            
            cluster_barplot <- renderPlotly({
                validate(
                    need(input$clusterButton >= 1 & !is.null(rv$tableCluster$cluster), message = FALSE),
                    errorClass = "cluster_barplot_err"
                )
                plot_ly(rv$clusterBar,
                        type = "bar",
                        x = as.factor(rv$clusterBar[,"Cluster no."]),   # no floats on x axis
                        y = rv$clusterBar[,"Cluster size"]) %>%
                    layout(title = "Number of items in each cluster",
                           xaxis = list(title = "Cluster no."),
                           yaxis = list(title = "Cluster size")
                    )
            })
            output$clusterBarplot <- cluster_barplot
            
        }
        if(input$cluster_alg == "Hierarchical") {
            validate(
                need(input$clusterButton >= 1 & input$cluster_alg == "Hierarchical", message = FALSE),
                errorClass = "hclust_err"
            )
            hclustplot <- renderPlot({
                rv$tableCluster$cluster = cutree(as.dendrogram(rv$hclust), input$hc_k)
                clusDendro <- dendrapply(as.dendrogram(rv$hclust), colLab)
                clusDendro %>% color_branches(k=input$hc_k, col = dendLabelColors[1:input$hc_k]) %>% plot(main = "Dendrogram",
                                                                                                          ylab = paste(input$hcmetric, "distance"))
                clusDendro %>% rect.dendrogram(k=input$hc_k, border = 8, lty = 5, lwd = 2)
                abline(h = heights_per_k.dendrogram(clusDendro)[input$hc_k] - .1, lwd = 2, lty = 2, col = "blue")
            })
            hclustHeights <- renderPlot({
                hghts <- rv$hclust$height
                i <- which.max(diff(rv$hclust$height))
                split_height <- (hghts[i]*0.7+hghts[i+1]*0.3)
                par(mfrow=c(1,2))
                plot(density((hghts)), main="Density of branching heights", xlab="", ylab="")
                abline(v = split_height, col="red", lty=2)
                
                seq <- max(0,floor(min(hghts))):floor(max(hghts))
                num <- sapply(seq, function(x){length(unique(cutree(rv$hclust,h=x)))})
                plot(seq, num, ylim=c(0,max(num)), xaxt="n", yaxt="n",
                     main="num of clusters (y) when cutting at height (x)",
                     xlab="", ylab="")
                text(seq, num, labels = num, cex= 1.3, pos=4)
                axis(1,at=seq)
                axis(2,at=0:max(num))
                abline(v = split_height, col="red", lty=2)
            })
            output$hclustplot <- hclustplot
            output$hclustHeights <- hclustHeights
        }
    })
    

    
    # Render clust algorithms
    clustTab <- renderUI({
        validate(
            need(length(input$cluster_alg) != 0, message = FALSE),
            errorClass = "cluster button err"
        )
        if(input$cluster_alg == "Hierarchical") {
            material_card(
                plotOutput("hclustplot"),
                plotOutput("hclustHeights")
            )
        } else ({ 
            material_card(
                plotlyOutput("clusterTable"),
                plotlyOutput("clusterBarplot"),
                DT::dataTableOutput("clusterUserTable")
            )
        })
    })
    
    # Classification
    observeEvent(input$confMat, {
        confusion_matrix <- DT::renderDataTable({
            validate(
                need(input$confMat >= 1 & !is.null(rv$tableCluster$cluster), message = FALSE),
                errorClass = "cluster_barplot_err"
            )
            dt <- rv$userTable[sapply(rv$userTable, is.numeric)]
            dt["Cluster"] <- as.factor(rv$tableCluster$cluster)
            testidx <- which(1:length(dt[,1])%%5 == 0)
            rvtrain <- dt[-testidx,]
            rvtest <- dt[testidx,]
            model <- randomForest(Cluster~., data=rvtrain, nTree = 50000)
            prediction <- predict(model, newdata=rvtest, type="class")
            dt <- as.data.frame.matrix(table(prediction, rvtest$Cluster))
            colnames(dt) <- levels(rv$tableCluster$cluster)
            dt <- cbind("Cluster no." = as.numeric(levels(rv$tableCluster$cluster)), dt)
            
            datatable(dt,
                      rownames = FALSE,
                      selection = list(target = 'none'),
                      filter = "none",
                      options = list(dom = "t"))
            
        })
        output$confusion_matrix <- confusion_matrix
    })  
    
    output$text_caution <- renderUI({
        removeClass(id = "text_caution", class = "greentext")
        removeClass(id = "text_caution", class = "redtext")
        
        rv$output <- paste("Everything is OK!")
        if(any(apply(rv$userTable, 2, function(x) any(is.na(x))))) {
            rv$output <- paste("Careful! You have NA values in dataset.")
            toggleClass(id = "text_caution", class = "redtext")
        }
        if(rv$output == paste("Everything is OK!")) {
            toggleClass(id = "text_caution", class = "greentext")
        }
        tags$i(rv$output)
    })
    output$fileUploadedBool <- reactive({
        return(!is.null(user_table()))
    })
    outputOptions(output, 'fileUploadedBool', suspendWhenHidden = FALSE)
    
    # pca
    observeEvent(input$clusterButton, {
        data <- rv$userTable[sapply(rv$userTable, is.numeric)]
        data <- scale(data)
        rv$pca_prep <- prcomp(data, scale. = TRUE)
        rv$pca_output <- as.data.frame(rv$pca_prep$x)
    })
    plotlyPCA_explained <- renderPlotly({
        validate(
            need(input$pcaButton >= 1, message = FALSE),
            errorClass = "PCA_plotly_err"
        )
        numeric_order_barNames <- factor(colnames(rv$pca_prep$x), levels=colnames(rv$pca_prep$x))
        pca_explained <- rv$pca_prep$sdev^2 / sum(rv$pca_prep$sdev^2)
        plot_ly(type = "bar",
                x = numeric_order_barNames,
                y = pca_explained) %>%
            layout(title = "Percentage of explained variance",
                   hovermode = 'x')
    })
    plotlyPCA <- renderPlotly({
        validate(
            need(input$pcaButton >= 1, message = FALSE),
            errorClass = "PCA_plotly_err"
        )
        df <- rv$pca_output
        plot_ly(df,
                type = "scatter",
                x = df[,input$pca_1],
                y = df[,input$pca_2],
                opacity = 0.8,
                color = as.factor(rv$tableCluster$cluster)) %>%
            layout(title = "PCA")
    })
    PCtable <- renderDataTable({
        validate(
            need(input$pcaButton >= 1, message = FALSE),
            errorClass = "PCA_plotly_err"
        )
        if(length(rv$pca_output) > 4){
        df <- rv$pca_output[,1:4]
        } else { df <- rv$pca_output}
        datatable(df,
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
    output$PCtable <- PCtable
    output$button_cluster_type <- button_cluster_type
    output$classification_buttons <- classification_buttons
    output$cluster_buttons <- button_cluster
    output$clustTab <- clustTab
    output$graph_buttons <- button_graph
    output$handsontypes <- types_table
    output$main_user_table <- main_user_table
    output$render_button <- button_render
    output$pca_explained <- plotlyPCA_explained
    output$pca_buttons <- pca_buttons
    output$plotlyPCA <- plotlyPCA
    output$plotlyGraph <- plotlygraph
}

sketch = htmltools::withTags(
    div(style = "
            position: relative;
            padding-left: 50px;
            overflow-x: auto;",
        class = "table-container",
        div(style = "
                    text-align: center;",
            class = "table-horizontal-header", "horizont"),
        div(style = "
                position: absolute;
                right: 0;
                top: 0;
                left: 0;
                width: 50px;
                bottom: 0;",
            class = "table-vertical-header", 
            span(style = "
                 display: block;
                 position: absolute;
                 transform: rotate(-90deg) translateX(50%);
                 text-align: center;
                 top: 50%;
                 height: 50px;",
                 "vertical")),
        div(class = "table-wrapper",
            table(
                class = "captions-enable",
                thead(tr(lapply(as.list(colnames(mtcars)), tags$th)))
            )
        )
    )
)


datatable(mtcars[1:20,], container = sketch, rownames = FALSE)
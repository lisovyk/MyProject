sketch = htmltools::withTags(table(
    class = "captions-enable",
    caption(
        style = "text-align: left;
        margin-top: 36px;
        margin-left: -44px;
        position: absolute;
        text-align: center;
        transform: rotate(-90deg);
        -webkit-transform: rotate(-90deg);
        -moz-transform: rotate(-90deg);
        -o-transform: rotate(-90deg);
        -ms-transform: rotate(-90deg);",
        "vertical"),
    caption(
        style = "text-align: left;
        margin-left: 50px;
        margin-top: 10px",
        "horizontal"),
    div(thead(tr(lapply(as.list(colnames(iris)), tags$th)))
    )
))

datatable(iris[1:20,], container = sketch, rownames = FALSE)



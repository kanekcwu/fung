server <- function(input, output, session) {
  output$weather <- DT::renderDataTable({
    weather_suggestions %>%
      datatable(
        class = "compact display",
        selection = list(mode = "single"),
        rownames = FALSE,
        escape = FALSE,
        options = list(
          dom = "t",
          pageLength = 100,
          ordering = FALSE
        )
      )
  })
  
  output$trending <- DT::renderDataTable({
    trending_suggestions %>%
      datatable(
        class = "compact display",
        selection = list(mode = "single"),
        rownames = FALSE,
        escape = FALSE,
        options = list(
          dom = "t",
          pageLength = 100,
          ordering = FALSE
        )
      )
  })
  
  output$product_drop <- DT::renderDataTable({
    drop_product %>%
      datatable(
        class = "compact display",
        selection = list(mode = "single"),
        rownames = FALSE,
        escape = FALSE,
        options = list(
          dom = "t",
          pageLength = 100,
          ordering = FALSE
        )
      )
  })
}

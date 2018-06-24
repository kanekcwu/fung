dashboardPage(
  dashboardHeader(
    title = img(src = "logo.png", height = "45px")
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    h3(sprintf("PROMO SUGGESTIONS OF %s", Sys.Date())),
    fluidRow(
      column(
        6,
        box(
          img(src = "today_weather.png", height = "60px"),
          br(),
          title = img(src = "weather.png", width = "30px", "Weather"),
          dataTableOutput("weather"),
          status = "primary", width = 12
        ),
        box(
          title = img(src = "sales_drop.jpg", "Product Sales Warning", width = "40px"),
          dataTableOutput("product_drop"),
          status = "warning", width = 12
        )
      ),
      column(
        6,
        box(
          lapply(trending, function(x)
            tags$span(x, class = "badge bg-red", style = "font-size:16px;padding:5px")),
          br(),
          br(),
          dataTableOutput("trending"),
          title = img(src = "google.png", width = "50px", "Google Trends"),
          status = "success", width = 12
        )
      )
    )
  ),
  title = "Micro Moment Maker"
)


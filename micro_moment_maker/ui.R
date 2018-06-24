dashboardPage(
  dashboardHeader(
    title = "Micro Moment Maker"
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
          title = "Sales Drop",
          status = "warning", width = 12
        )
      ),
      column(
        6,
        box(
          lapply(trending, function(x)
            tags$span(x, class = "badge bg-red", style = "font-size:24px;padding:5px")),
          br(),
          br(),
          dataTableOutput("trending"),
          title = img(src = "google.png", width = "50px", "Google Trends"),
          status = "success", width = 12
        )
      )
    )
  )
)


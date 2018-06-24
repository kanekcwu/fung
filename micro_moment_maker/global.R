library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

weather_suggestions <- tibble(
  `Sugguested Category` = c("Cooling Drink", "Carbonated Drinks", "Tea"),
  `Target Audience` = c("Office Worker", "Office Worker, Housewife", "Night Person"),
  `Expected Sales Increase` = c("50%", "40%", "30%")
)

trending <- c("Sandwich", "Ice cream", "world cup", "Lychee", "Sparkling wine")

trending_suggestions <- tibble(
  `Sugguested Category` = c("Ice Cream", "Magazine", 
                            'Sparkline Wine <span class="badge bg-blue">new</span>'),
  `Target Audience` = c("Office Worker", "Office Worker, Housewife", "Night Person"),
  `Expected Sales Increase` = c("50%", "40%", "")
)

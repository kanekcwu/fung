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
                            'Sparkling Wine <span class="badge bg-blue">new</span>'),
  `Target Audience` = c("Office Worker", "Office Worker, Housewife", "Night Person"),
  `Expected Sales Increase` = c("50%", "40%", "")
)

load("drop_product.rda")

drop_product <- drop_product %>%
  mutate(drop = scales::percent(drop)) %>%
  rename(Product = product,
         `This week sales` = p2,
         `Last week sales` = p1,
         `Sales Drop` = drop)

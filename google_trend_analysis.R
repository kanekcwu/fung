#-----------------------------------------------------------------------------#
# How Google Trends affect sales
#-----------------------------------------------------------------------------#
library(gtrendsR)

gtrends_category <- function(id, date) {
  date <- as.Date(date)
  date1 <- date - 7
  date_s <- sprintf("%s %s", date1, date)
  res <- gtrends(geo = "HK", time = date_s, category = id)
  trending_topics <- res$related_topics
  trending_queries <- res$related_queries
  list(
    trending_topics = trending_topics,
    trending_queries = trending_queries
  )
}

ids <- c(
  "Food & Drink" = 71,
  "Baked Goods" = 907,
  "Non-Alcoholic Beverages" = 560,
  "Cold & Flu" = 629,
  "Fast Food" = 918,
  "Gifts" = 99,
  "Toys" = 432,
  "Candy & Sweets" = 906,
  "Alcoholic Beverages" = 277
)

# Get trends as of April 30
data <- list()
for (id in names(ids)) {
  data[[id]] <- gtrends_category(ids[id], "2018-06-24")
}

trending_topics <- list()
trending_queries <- list()
for (id in names(data)) {
  tt <- data[[id]][["trending_topics"]]
  tt$category <- id
  tq <- data[[id]][["trending_queries"]]
  tq$category <- id
  trending_queries <- bind_rows(trending_queries, tq)
  trending_topics <- bind_rows(trending_topics, tt)
}


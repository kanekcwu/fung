#-----------------------------------------------------------------------------#
# Obtain realtime/historical weather data from open data and extract insight
# Explore holidy/weekend as well
#-----------------------------------------------------------------------------#

library(tidyverse)
library(rvest)
library(httr)
library(lubridate)

# Utility functions to obtain historical weather data from open data source----
get_weather_dt <- function(date, time) {
  date1 <- format(as.Date(date), "%Y%m%d")
  time <- sprintf("%s-%s", date1, time)
  res <-  GET(
    "https://api.data.gov.hk/v1/historical-archive/get-file",
    query = list(url = "http://rss.weather.gov.hk/rss/CurrentWeather.xml",
                 time = time))
  res_t <- content(res, "text", encoding = "UTF-8")
  temp <- gsub(paste0(".+Air temperature : ([[:digit:]]+)",
                      " degrees Celsius<br/>.+"), "\\1", res_t)
  hum <- gsub(paste0(".+Relative Humidity : ([[:digit:]]+)",
                     " per cent<br/>.+"), "\\1", res_t)
  tibble(
    date = as.Date(date),
    temp = as.numeric(temp),
    hum = as.numeric(hum)
  )
}

get_weather <- function(date) {
  # get average of 3 times
  d <- do.call("bind_rows", lapply(
    c("0900", "1300", "1800"), get_weather_dt, date = date
  ))
  d %>%
    filter(!is.na(temp)) %>%
    group_by(date) %>%
    summarise_all(funs(mean))
}

# Get the weather data in april------------------------------------------------
april_weather <- do.call(
  "bind_rows", lapply(
    as.Date("2018-04-01") + 0:29, get_weather
  ))

# Find correlation with categories in transaction data-------------------------
# Find the most significant correlation
trans <- read_csv("data/transactions.csv")

trans_sum <- trans %>% 
  mutate(n = as.numeric(`Sold Qty`)) %>%
  group_by(Date, Cat) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  inner_join(april_weather, by = c("Date" = "date"))

# Holiday or weekends
trans_sum <- trans_sum %>%
  mutate(holiday_weekend = weekdays(Date) %in% c("Saturday", "Sunday") |
           Date %in% as.Date("2018-04-05"),
         holiday_weekend = as.numeric(holiday_weekend))

# Possible pay day
trans_sum <- trans_sum %>%
  mutate(payday = Date >= "2018-04-25" | Date <= "2018-04-05")

# Correlation metric between category and temperare and humidity
cors <- list(
  temp = c(),
  hum = c(),
  holiday_weekend = c(),
  payday = c()
)

for (d in unique(trans_sum$Cat)) {
  if (is.na(d)) return()
  dt <- trans_sum %>% filter(Cat == d)
  # Temperature
  fit <- lm(n~temp, data = dt)
  cors$temp[d] <- summary(fit)$coefficients[2, 3]
  # Humidity
  fit <- lm(n~hum, data = dt)
  cors$hum[d] <- summary(fit)$coefficients[2, 3]
  # Holiday weekend
  fit <- lm(n~holiday_weekend, data = dt)
  cors$holiday_weekend[d] <- summary(fit)$coefficients[2, 3]
  # Payday
  fit <- lm(n~payday, data = dt)
  cors$payday[d] <- summary(fit)$coefficients[2, 3]
}

# Top correlations with temperature
sort(cors$temp[abs(cors$temp) > 3], decreasing = TRUE)

sort(cors$hum[abs(cors$hum) > 3], decreasing = TRUE)

sort(cors$holiday_weekend[abs(cors$holiday_weekend) > 3], decreasing = TRUE)

sort(cors$payday[abs(cors$payday) > 3], decreasing = TRUE)


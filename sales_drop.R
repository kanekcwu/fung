#-----------------------------------------------------------------------------#
# FIND WHAT PRODUCTS HAVE SIGNIFICANT SALES DROP
#-----------------------------------------------------------------------------#

library(tidyverse)

trans <- read_csv("data/transactions.csv")

# Split time into 2 periods: 0424-0430 and 0417-0423
trans_sum <- trans %>%
  rename(product = `Prod Name (Chi)`) %>%
  filter(between(Date, as.Date("2018-04-17"), as.Date("2018-04-30"))) %>%
  mutate(period = ifelse(
    between(Date, as.Date("2018-04-17"), as.Date("2018-04-23")),
    "p1", "p2"
  ))

trans_sum <- trans_sum %>%
  group_by(product, period) %>%
  summarise(total = sum(`Sold Qty`, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(key = period, value = total, fill = 0)

drop_product <- trans_sum %>%
  mutate(drop = (p2 - p1) / p1) %>%
  filter(drop <= -0.5, p1 >= 100)
  
  

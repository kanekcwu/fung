#-----------------------------------------------------------------------------#
# FIND WHAT PRODUCTS HAVE SIGNIFICANT SALES DROP
#-----------------------------------------------------------------------------#

library(tidyverse)

trans <- read_csv("data/transactions.csv")

# Split time into 2 periods: 0424-0430 and 0417-0423
trans_sum <- trans %>%
  filter(between(Date, as.Date("2018-04-17"), as.Date("2018-04-30"))) %>%
  mutate(period = ifelse(
    between(Date, as.Date("2018-04-17"), as.Date("2018-04-23")),
    "p1", "p2"
  ))
library(tidyverse)
library(lubridate)
library(janitor)
library(dummies)
library(factoextra)
library(cluster)
library(mclust)

# feature engineering =========================================================

# read csv
trans <- read_csv('data/transactions.csv')
memtrans <- trans %>% filter(!is.na(`Member ID`))

# start creating member profiles
memprofile <- memtrans %>% 
  select(-`Member Birthday`, -`Prod Name (Chi)`, 
         -`Cat`, -`Subcat`) %>% 
  clean_names()

# total amount
memprofile <- memprofile %>% 
  group_by(member_id) %>% 
  mutate(total_amount = sum(amount))

# filter discounts
memprofile <- memprofile %>% 
  filter(amount > 0)

# store type
store_type <- memprofile %>%
  group_by(member_id, store_type) %>%
  summarize(n = n_distinct(trans_id)) %>%
  group_by(member_id) %>%
  mutate(p = n / sum(n)) %>%
  select(-n) %>%
  spread(key = store_type, value = p, fill = 0) %>% 
  clean_names()

# mop
mop <- memprofile %>%
  group_by(member_id, mop) %>%
  summarize(n = n_distinct(trans_id)) %>%
  group_by(member_id) %>%
  mutate(p = n / sum(n)) %>%
  select(-n) %>%
  spread(key = mop, value = p, fill = 0)
names(mop) <- c('member_id', 'cup', 'eps', 'octopus', 'tng', 
                'visa_master', 'weixin', 'alipay', 'cash')

# dept
dept <- memprofile %>%
  group_by(member_id, dept) %>%
  summarize(n = sum(sold_qty)) %>%
  group_by(member_id) %>%
  mutate(p = n / sum(n)) %>%
  select(-n) %>%
  spread(key = dept, value = p, fill = 0) %>% 
  clean_names()

# store_id
store_id <- memprofile %>%
  group_by(member_id, store_id) %>%
  summarize(n = n_distinct(trans_id)) %>%
  group_by(member_id) %>%
  mutate(p = n / sum(n)) %>%
  select(-n) %>%
  spread(key = store_id, value = p, fill = 0)
names(store_id) <- c('member_id', sprintf('store_%s', names(store_id)[-1]))

# hour
memprofile <- memprofile %>% 
  mutate(hour = str_sub(time, 1, 2))
period <- memprofile %>%
  group_by(member_id, hour) %>%
  summarize(n = n_distinct(trans_id)) %>%
  mutate(period = ifelse(hour %in% c('06', '07'), 'early_morning', 
                         ifelse(hour %in% c('08', '09', '10'), 'morning', 
                                ifelse(hour %in% c('11', '12', '13', '14'), 'lunch', 
                                       ifelse(hour %in% c('15', '16', '17'), 'afternoon', 
                                              ifelse(hour %in% c('18', '19', '20', '21', '22', '23'), 
                                                     'evening', 'latenight')))))) %>% 
  select(-hour) %>% 
  group_by(member_id, period) %>% 
  summarize(n = sum(n)) %>% 
  mutate(p = n / sum(n)) %>% 
  select(-n) %>%
  spread(key = period, value = p, fill = 0)

# weekday
memprofile <- memprofile %>% 
  mutate(weekday = weekdays(as.POSIXlt(date)))
weekday <- memprofile %>%
  group_by(member_id, weekday) %>%
  summarize(n = n_distinct(trans_id)) %>%
  group_by(member_id) %>%
  mutate(p = n / sum(n)) %>%
  select(-n) %>%
  spread(key = weekday, value = p, fill = 0)

# join 1
memprofile <- memprofile %>%
  select(member_id, member_grade, gender, total_amount, weekday) %>%
  unique() %>%
  left_join(store_type, by = "member_id") %>%
  left_join(mop, by = "member_id") %>%
  left_join(dept, by = "member_id") %>%
  left_join(period, by = "member_id") %>%
  left_join(store_id, by = "member_id") %>%
  left_join(weekday, by = "member_id")

# join 2
memprofile1 <- memprofile %>% 
  mutate(v = 1) %>% 
  mutate(member_grade = ifelse(is.na(member_grade), 'NA', member_grade)) %>%
  spread(key = member_grade, value = v, fill = 0) %>%
  mutate(v = 1) %>% 
  mutate(gender = ifelse(is.na(gender), 'no_gender', 
                         ifelse(gender == 'F', 'female', 'male'))) %>%
  spread(key = gender, value = v, fill = 0) %>%
  select(-weekday)

# join 3
memprofile2 <- memprofile1 %>% 
  ungroup() %>% 
  unique() %>% 
  select(-total_amount, -member_id)

rm(dept, period, mop, store_id, store_type, weekday, memtrans, trans)

# clustering ==================================================================

# period cluster --------------------------------------------------------------
period_profile <- memprofile2 %>% 
  select(afternoon:lunch)

cluster_period1 <- clara(period_profile, k = 6, metric = 'euclidean', 
                         pamLike = TRUE)

fviz_cluster(cluster_period1,
             ellipse.type = 't',
             geom = 'point', 
             pointsize = 1,
             ggtheme = theme_classic())

memprofile2$period_cluster <- cluster_period1$clustering

# store type cluster ----------------------------------------------------------
store_type_profile <- memprofile2 %>% 
  select(hpr_private_housing:tpr_transport_pt_residential)

cluster_type1 <- clara(store_type_profile, k = 7, metric = 'euclidean', 
                         pamLike = TRUE)

fviz_cluster(cluster_type1,
             ellipse.type = 't',
             geom = 'point', 
             pointsize = 1,
             ggtheme = theme_classic())

memprofile2$type_cluster <- cluster_type1$clustering

# dept cluster ----------------------------------------------------------------
dept_profile <- memprofile2 %>% 
  select(a_p_expense:wine_spirit)

# clara
cluster_dept1 <- clara(dept_profile, k = 10, metric = 'euclidean',
                       pamLike = TRUE)

fviz_cluster(cluster_dept1,
             ellipse.type = 't',
             geom = 'point',
             pointsize = 1,
             ggtheme = theme_classic())


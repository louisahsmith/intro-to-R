library(tidyverse)

colnames_nlsy <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
                   "id", "nsibs", "samp", "race_eth", "sex", "region", 
                   "income", "res_1980", "res_2002", "age_bir")

nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"), 
                 col_names = colnames_nlsy, skip = 1,
                 na = c("-1", "-2", "-3", "-4", "-5", "-999", "-998"))

nlsy_clean <- nlsy %>%
  filter(income < 30000) %>%
  mutate(
    region = factor(region, labels = c("Northeast", "North central", 
                                       "South", "West")),
    sex = factor(sex, labels = c("Male", "Female")),
    log_inc = ifelse(income > 0, log(income), NA),
    eyesight = factor(eyesight, labels = c("Excellent", "Very Good", "Good", 
                                           "Fair", "Poor")),
    race_eth = factor(race_eth, labels = c("Hispanic", "Non-Hispanic Black", 
                                           "Non-Black, Non-Hispanic"))
  ) %>%
  select(id, region, sex, log_inc, eyesight, income,
         race_eth, nsibs, age_bir, contains("sleep")) %>%
  na.omit()

write_rds(nlsy_clean, here::here("data", "processed", "nlsy_clean.rds"))

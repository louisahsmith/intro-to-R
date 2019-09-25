#====================================================================#
# Title: Day 3                                                       #
# Name:  Your name here                                              #
#====================================================================#

#### Code ------------------------------------------------------------
# Run through this code on your own to practice what we covered in
# the slides. Today I've put it all in one script, so keep track of 
# where you are! You can always restart R and rerun to start from 
# scratch if you've messed anything up.

# Before you do anything, make sure you're starting in a fresh session.
# Restart R by going to Session -> Restart R
# You can make it so that this also clears your workspace (highly recommended)
# by going to Tools -> Global Options -> General -> Basic -> Workspace
# and unclicking "restore .RData" and switching the other option to "never"

# Then we need to reload packages and data (same as last week)
library(tidyverse)
nlsy <- read_csv("nlsy_cc.csv")
colnames(nlsy) <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
                    "id", "nsibs", "samp", "race_eth", "sex", "region", 
                    "income", "res_1980", "res_2002", "age_bir")

#### pipes -----------------------------------------------------

# compare:
nlsy2 <- mutate(nlsy, only = case_when(
  nsibs == 0 ~ "yes",
  TRUE ~ "no"))
nlsy3 <- select(nlsy2, 
                id, contains("sleep"), only)
only_kids <- filter(nlsy3, only == "yes")
only_kids

# to:
only_kids <- nlsy %>%
  mutate(only = case_when(
    nsibs == 0 ~ "yes",
    TRUE ~ "no")) %>%
  select(id, contains("sleep"), only) %>%
  filter(only == "yes")
only_kids

# instructions to follow in exercises
nlsy <- mutate(nlsy, slp_cat_wkdy = case_when(
  sleep_wkdy < 5 ~ "little",
  sleep_wkdy < 7 ~ "some",
  sleep_wkdy < 9 ~ "ideal",
  sleep_wkdy < 12 ~ "lots",
  TRUE ~ NA_character_))
missing_sleep <- filter(nlsy, is.na(slp_cat_wkdy))
missing_sleep <- select(missing_sleep, starts_with("slp"), 
                        contains("sleep"))
missing_sleep

#### Exercises 1 -----------------------------------------------------

# 1. Rewrite the code above using pipes. Make sure you get the same dataset when your code runs!
# Follow the instructions on the previous slide using pipes. (The code on the previous slide is provided in today's script.)
# Answer:
missing_sleep <- nlsy %>%
  mutate(slp_cat_wkdy = case_when(
    sleep_wkdy < 5 ~ "little",
    sleep_wkdy < 7 ~ "some",
    sleep_wkdy < 9 ~ "ideal",
    sleep_wkdy < 12 ~ "lots",
    TRUE ~ NA_character_
  )) %>%
  filter(is.na(slp_cat_wkdy)) %>%
  select(starts_with("slp"), contains("sleep"))
missing_sleep

# 2. Experiment with switching up the "order of operations". Can you complete
# the instructions in a different order and get the same result? Can you think
# of a situation when you might not be able to do so?
# Answer:
# We could switch around select and filter, since we are filtering based
# on one of the variables we were selecting. We could also move select to
# the beginning, since the variable we create only depends on variables
# we select.

#### summarize -----------------------------------------------------

summarize(nlsy, 
          med_age_bir = median(age_bir),
          cor_sleep = cor(sleep_wkdy, sleep_wknd),
          ten_pctle_inc = quantile(income, probs = 0.1),
          ninety_pctle_inc = quantile(income, probs = 0.9))

# always helps to read the documentation when you learn a new function!
help(summarize)
# note in the documentation you can also spell it summarise!

nlsy %>% summarize(q.1 = quantile(age_bir, probs = 0.1),
                   q.2 = quantile(age_bir, probs = 0.2),
                   q.3 = quantile(age_bir, probs = 0.3),
                   q.4 = quantile(age_bir, probs = 0.4),
                   q.5 = quantile(age_bir, probs = 0.5))

# put several functions together with pipes
nlsy %>%
  mutate(age_bir_stand = (age_bir - mean(age_bir)) / sd(age_bir)) %>%
  filter(sex == 1) %>%
  summarize(mean_men = mean(age_bir_stand))


#### Exercises 2 -----------------------------------------------------

# 1. Among the only children, find the correlation between hours of sleep on
# weekdays and weekends.
# Answer:
nlsy %>%
  filter(nsibs == 0) %>%
  summarize(sleep_cor = cor(sleep_wkdy, sleep_wknd))

# 2. Create a variable that is 1 if an observation has an income between 20,000
# and 30,000, and 0 otherwise. Calculate the proportion of people in the
# dataset who fit that criterion.
# Answer:
nlsy %>%
  mutate(mid_inc = between(income, 20000, 30000)) %>%
  summarize(mid_inc_prop = mean(mid_inc))

# 3. Recreate the summary() function using summarize() (i.e., produce all the
# same statistics for a variable of your choice.)
# Answer:
summary(nlsy$income)
nlsy %>%
  summarise(Min. = min(income),
            `1st Qu.` = quantile(income, probs = .25),
            Median = median(income),
            Mean = mean(income),
            `3rd Qu.` = quantile(income, probs = .75),
            Max. = max(income))


#### group_by -----------------------------------------------------

# grouped dataset
nlsy %>%
  mutate(income_stand = (income - mean(income))/sd(income)) %>%
  select(id, region, income_stand, race_eth, sex) %>%
  group_by(race_eth)

# stratified summary stats
nlsy %>%
  mutate(income_stand = (income - mean(income))/sd(income)) %>%
  group_by(region) %>%
  summarize(mean_inc = mean(income_stand),
            sd_inc = sd(income_stand))

# multiple grouping variables
nlsy %>%
  group_by(region, sex) %>%
  summarize(mean_inc = mean(income),
            sd_inc = sd(income))

# counting observations in a group
nlsy %>%
  group_by(sex) %>%
  summarize(n = n())

nlsy %>%
  group_by(sex) %>%
  tally()

nlsy %>% 
  count(sex)

# calculating proportions
nlsy %>%
  group_by(sex) %>%
  summarize(n = n()) %>%
  mutate(prop = n / sum(n))

#### Exercises 3 -----------------------------------------------------

# 1. Find the median income per region. Before doing so, make sure that you've
# made region into a factor variable with appropriate names so we can easily
# read your results.
# Answer:
nlsy %>%
  mutate(region = factor(region, labels = c("Northeast", "North Central", 
                                            "South", "West"))) %>%
  group_by(region) %>%
  summarise(med_inc = median(income))

# 2. Calculate and compare the median income for people who sleep at least 8
# hours on weekdays and those who sleep less. 
# Answer:
nlsy %>%
  mutate(sleep_cat = case_when(
    sleep_wkdy >= 8 ~ "lots",
    sleep_wkdy < 8 ~ "little"
  )) %>%
  group_by(sleep_cat) %>%
  summarise(med_inc = median(income))


# 3. Among the women (sex = 2), calculate the proportion who live in each
# region.
# Answer:
nlsy %>%
  mutate(region = factor(region, labels = c("Northeast", "North Central", 
                                            "South", "West"))) %>%
  filter(sex == 2) %>%
  count(region) %>%
  mutate(prop = n/sum(n))

#### table 1 -----------------------------------------------------

install.packages(tableone)
library(tableone) # in general, put at the top of your script

tab1 <- CreateTableOne(
  data = nlsy,
  vars = c("eyesight", "nsibs", "race_eth",
           "sex", "region", "income", "age_bir"),
  strata = "glasses",
  factorVars = c("eyesight", "race_eth", "sex", "region")
)

tab1

# change some of the options
print(tab1, catDigits = 2, contDigits = 2, test = FALSE, smd = TRUE)

help("CreateTableOne")
help("print.TableOne")

# check out some other packages for tables: 
# https://github.com/kaz-yos/tableone#similar-or-complementary-projects
# try one out on your data this week!

#### Exercises 4 -----------------------------------------------------

# 1. Apply the following inclusion criteria to the NLSY dataset to make a
# analysis dataset: from region 1 or 4, with at least 2 siblings, and doesn't
# wear glasses.
# Answer:
dat <- nlsy %>%
  filter(region %in% c(1, 4),
         nsibs >= 2,
         glasses == 0)

# 2. Make a variable in that dataset that categorizes people in quartiles
# (split at 25th, 50th, and 75% percentiles of the new dataset) by income. Make
# sure the categories have descriptive names.
# Answer:
dat <- dat %>%
  mutate(income_cat = case_when(
    income < quantile(income, probs = .25) ~ "lowest quartile",
    income < quantile(income, probs = .5) ~ "second quartile",
    income < quantile(income, probs = .75) ~ "third quartile",
    TRUE ~ "highest quartile"),
    income_cat = fct_relevel(income_cat, "lowest quartile",
                             "second quartile", "third quartile",
                             "highest quartile"))

# 3. Make a table 1 for this new dataset, stratified by the new income
# variable. Make sure they show up in the correct order in your table.
# Include p-values testing across strata but only print 2 digits for
# them. Perform an exact test for the region comparison.
# Answer:
tab1 <- CreateTableOne(
  data = dat,
  vars = c("eyesight", "nsibs", "race_eth",
           "sex", "region", "income", "age_bir"),
  strata = "income_cat",
  factorVars = c("eyesight", "race_eth", "sex", "region")
)
print(tab1, exact = "region", pDigits = 2)

# Challenge. Make a nice-looking table 1 for some data you are working with or
# have access to!
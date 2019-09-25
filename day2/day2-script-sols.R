#====================================================================#
# Title: Day 2                                                       #
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

#### mutate -----------------------------------------------------
nlsy <- mutate(nlsy,
  region_factor = factor(region),
  age_bir_cent = age_bir - mean(age_bir),
  dataset = "NLSY"
)

nlsy_new <- mutate(nlsy,
  age_bir_cent = age_bir - mean(age_bir),
  age_bir_stand = age_bir_cent / sd(age_bir_cent)
)

nlsy <- mutate(nlsy, slp_cat_wkdy = case_when(
  sleep_wkdy < 5 ~ "little",
  sleep_wkdy < 7 ~ "some",
  sleep_wkdy < 9 ~ "ideal",
  sleep_wkdy < 12 ~ "lots",
  TRUE ~ NA_character_
))
# note that table doesn't show NAs! can be dangerous!
table(nlsy$slp_cat_wkdy, nlsy$sleep_wkdy)

nlsy <- mutate(nlsy, total_sleep = case_when(
  sleep_wknd > 8 & sleep_wkdy > 8 ~ 1,
  sleep_wknd + sleep_wkdy > 15 ~ 2,
  sleep_wknd - sleep_wkdy > 3 ~ 3,
  TRUE ~ NA_real_
))

#### Exercises 1 -----------------------------------------------------
# Answer the questions below. Remember to include your answers in
# this script so you can save and look back at it!

# 1. Using the NLSY data and `mutate()`, make a standardized (centered at the
# mean, and divided by the standard deviation) version of income.
# Answer:
nlsy <- mutate(nlsy,
  income_cent = income - mean(income),
  income_stand = income_cent / sd(income_cent)
)
summary(nlsy$income_stand)

# 2. Do the same thing, but using income on the log scale. Look at this
# variable using `summary()`. Can you figure out what happened? (Hint: look at
# log(income).)
# Answer:
nlsy <- mutate(nlsy,
  log_income_cent = log(income) - mean(log(income)),
  log_income_stand = log_income_cent / sd(log_income_cent)
)
summary(nlsy$log_income_stand)
# Some of the values of income were 0, so they were transformed to -Inf
# When we tried to take the mean of everything including those values,
# we get -Inf and -Inf - -Inf = NaN (not a number) according to R,
# then we can't take the standard deviation of that 

# 3. Redo question 2, but if you are not able to calculate log(income) for an
# observation, replace it with a missing value (using `case_when()`). This
# time, when you standardize log(income), you'll have to use `na.rm = TRUE` to
# remove missing values both when you take the mean and the standard deviation.
# Answer:

nlsy <- mutate(nlsy,
  log_income = case_when(
    income > 0 ~ log(income),
    TRUE ~ NA_real_
  ),
  log_income_cent = log_income - mean(log_income, na.rm = TRUE),
  log_income_stand = log_income_cent / sd(log_income_cent, na.rm = TRUE)
)
summary(nlsy$log_income_stand)

#### factors -----------------------------------------------------

nlsy <- mutate(nlsy, slp_cat_wkdy_ord = fct_relevel(
  slp_cat_wkdy,
  "little", "some", "ideal", "lots"
))

# misspelling
nlsy <- mutate(nlsy, slp_cat_wkdy_ord2 = fct_relevel(
  slp_cat_wkdy,
  "little", "same", "ideal", "lots"
))

nlsy <- mutate(nlsy,
  region_fact = factor(region),
  region_fact = fct_recode(region_fact,
    "Northeast" = "1",
    "North Central" = "2",
    "South" = "3",
    "West" = "4"
  )
)

nlsy <- mutate(nlsy, region_fact = fct_infreq(region_fact))

nlsy <- mutate(nlsy, region_fact = fct_rev(region_fact))

nlsy <- mutate(nlsy,
  slp_cat_wkdy_out =
    fct_explicit_na(slp_cat_wkdy, na_level = "outlier")
)

nlsy <- mutate(nlsy,
  slp_cat_wkdy_comb =
    fct_collapse(slp_cat_wkdy,
      "less" = c("little", "some"),
      "more" = c("ideal", "lots")
    )
)

nlsy <- mutate(nlsy, slp_cat_wkdy_lump = fct_lump(slp_cat_wkdy, n = 2))

#### Exercises 2 -----------------------------------------------------

# 1. Turn the eyesight variable into a factor variable. The numbers 1-5
# correspond to excellent, very good, good, fair, and poor. Make sure that
# categories are in an appropriate order.
# Answer:
nlsy <- mutate(nlsy,
  eyesight_fact = factor(eyesight),
  eyesight_fact = fct_recode(eyesight_fact,
    "Excellent" = "1",
    "Very Good" = "2",
    "Good" = "3",
    "Fair" = "4",
    "Poor" = "5"
  )
)
summary(nlsy$eyesight_fact)

# 2. Use two different methods to combine the worst two categories of eyesight
# into one category.
# Answer:
nlsy <- mutate(nlsy,
  eyesight_worst = fct_collapse(eyesight_fact,
    "Worst" = c("Fair", "Poor")
  )
)
summary(nlsy$eyesight_worst)
# can also use fct_lump since I know the two worst are the 
# two smallest categories
nlsy <- mutate(nlsy,
  eyesight_worst2 = fct_lump(eyesight_fact,
    n = 3,
    other_level = "Worst"
  )
)
summary(nlsy$eyesight_worst2)

# 3. Make a new categorical income variable with at least 3 levels (you can
# choose the cutoffs). Make a bar graph with this new variable where the bars
# are in the correct order from low to high and are colored increasingly dark
# shades of green. (Hint: http://colorbrewer2.org; `scale_color_brewer()`)
# Answer:

#### select -----------------------------------------------------

nlsy_subs <- select(nlsy, id, income, eyesight, sex, region)
nlsy_subs

select(nlsy_subs, -id, -region)

cols_I_want <- c("age_bir", "nsibs", "region")
select(nlsy, one_of(cols_I_want))

select(nlsy, starts_with("slp"))

select(nlsy, id, everything())

#### Exercises 3 -----------------------------------------------------

# 1. Create mean-centered versions of "age_bir", "nsibs", "income", and the two
# sleep variables. Use the same ending (e.g., "_cent") for all of them. Then
# make a new dataset of just the centered variables using `select()` and a
# helper.
# Answer:
nlsy <- mutate(nlsy,
               age_bir_cent = age_bir - mean(age_bir),
               nsibs_cent = nsibs - mean(nsibs),
               income_cent = income - mean(income),
               sleep_wkdy_cent = sleep_wkdy - mean(sleep_wkdy),
               sleep_wknd_cent = sleep_wknd - mean(sleep_wknd))

cent_vars <- select(nlsy, ends_with("cent"))
cent_vars

# 2. You may have added a lot of variables to the original dataset by now.
# Create a dataset called `nlsy_orig` that contains only the variables we
# started off with, using the vector of names we originally used to name the
# columns and the `one_of()` helper.
# Answer:
colnames_orig <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
                   "id", "nsibs", "samp", "race_eth", "sex", "region", 
                   "income", "res_1980", "res_2002", "age_bir")
nlsy_orig <- select(nlsy, one_of(colnames_orig))
nlsy_orig

# 3. Look at `help(select)`. You'll notice that `rename()` is a related
# function. Looking at the examples to help, rename "age_bir" to
# "age_1st_birth" without making a new column.
# Answer:
nlsy <- rename(nlsy, age_1st_birth = age_bir)
nlsy

#### filter -----------------------------------------------------

wear_glasses <- filter(nlsy, glasses == 1)

yesno_glasses <- filter(nlsy, glasses == 0, glasses == 1)

glasses_great_eyes <- filter(nlsy, glasses == 1, eyesight == 1)

extreme_eyes <- filter(nlsy, eyesight == 1 | eyesight == 5)

some_regions <- filter(
  nlsy,
  region_fact == "Northeast" |
    region_fact == "South"
)

more_regions <- filter(nlsy, region_fact %in%
  c("South", "West", "Northeast"))

# other examples... play around with it!
7 %in% c(4, 6, 7, 10)
5 %in% c(4, 6, 7, 10)
!7 %in% c(4, 6, 7, 10)
!5 %in% c(4, 6, 7, 10)

northcentralers <- filter(
  nlsy,
  !region_fact %in% c("South", "West", "Northeast")
)

# compare
my_data <- filter(
  nlsy,
  age_bir_cent < 1,
  sex != 1,
  nsibs %in% c(1, 2, 3),
  !is.na(slp_cat_wkdy)
)
summary(select(my_data, age_bir_cent, sex, nsibs, slp_cat_wkdy))

oth_dat <- filter(
  nlsy,
  (age_bir_cent < 1) &
    (sex != 1 | nsibs %in% c(1, 2, 3)) &
    !is.na(slp_cat_wkdy)
)
summary(select(oth_dat, age_bir_cent, sex, nsibs, slp_cat_wkdy))

#### Exercises 4 -----------------------------------------------------

# 1. Create a dataset with all the observations that get over 7 hours of sleep
# on both weekends and weekdays *or* who have an income greater than/equal to
# 20,000 and less than/equal to 50,000.
# Answer:
newdat <- filter(nlsy,
                 (sleep_wkdy > 7 & sleep_wknd > 7) |
                   (income >= 20000 & income <= 50000))


# 2. Create a dataset that consists *only* of the missing values in
# `slp_cat_wkdy`. Check how many rows it has (there should be 3!).
# Answer:
newdat2 <- filter(nlsy, is.na(slp_cat_wkdy))
nrow(newdat2)

# 3. Look up the `between()` function in help. Figure out how to use this to
# answer question 1, when choosing people whose income is between 20,000 and
# 50,000. Check to make sure you get the same number of rows.
# Answer:
newdat3 <- filter(nlsy,
                 (sleep_wkdy > 7 & sleep_wknd > 7) |
                   between(income, 20000, 50000))
nrow(newdat)
nrow(newdat3)

#### Challenge -----------------------------------------------------

# start here!
nlsy_full <- read_rds("nlsy.rds")
colnames(nlsy_full) <- colnames_orig
levels(nlsy_full$res_1980)
levels(nlsy_full$res_2002)

nlsy_full <- mutate(nlsy_full,
  # fct_relabel -> tolower makes all the levels lowercase
  res_1980_new = fct_relabel(res_1980, tolower),
  res_2002_new = fct_relabel(res_2002, tolower),
  # rename all the mismatched labels
  res_2002_new = fct_recode(res_2002_new,
    "aboard ship, barracks" = "open bay or troop barracks, aboard ship",
    "bachelor, officer quarters" = "bachelor enlisted or officer quarters",
    "dorm, fraternity, sorority" = "dormitory, fraternity or sorority",
    "other temporary quarters" = "other temporary individual quarters",
    "on-base mil fam housing" = "on-base military family housing",
    "off-base mil fam housing" = "off-base military family housing",
    "religious institution" = "convent, monastery, other religious institute",
    "r in parental household" = "respondent in parent household"
  ),
  res_1980_new = fct_collapse(res_1980_new,
    "r in parental household" = c("r in parental household", "parental")
  ),
  res_2002_new = fct_expand(res_2002_new,
    "orphanage", "hhi conducted with parent"
  ),
  res_1980_new = fct_infreq(res_1980_new),
  # here fct_relevel will just rearrange the levels in the same order 
  # as the levels of the nlsy_res version
  res_2002_new = fct_relevel(res_2002_new, levels(res_1980_new))
)

nlsy_res <- select(nlsy_full, ends_with("_new"))
nlsy_res <- filter(nlsy_res, !is.na(res_1980_new), !is.na(res_2002_new))
nlsy_res
# still have the same levels
# that means they'll be easy to combine/compare
levels(nlsy_res$res_1980_new)
levels(nlsy_res$res_2002_new)
table(nlsy_res$res_1980_new, nlsy_res$res_2002_new)


# Exercises 1

# here's the code from the first part
nlsy <- mutate(nlsy, slp_cat_wkdy = case_when(
  sleep_wkdy < 5 ~ "little",
  sleep_wkdy < 7 ~ "some",
  sleep_wkdy < 9 ~ "ideal",
  sleep_wkdy < 12 ~ "lots",
  TRUE ~ NA_character_))
missing_sleep <- filter(nlsy, is.na(slp_cat_wkdy))
missing_sleep <- select(missing_sleep, starts_with("slp"), contains("sleep"))
missing_sleep

# 1. Rewrite the code above using pipes. Make sure you get the same dataset when your code runs!
# 
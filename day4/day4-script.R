#====================================================================#
# Title: Day 4                                                       #
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
library(tableone)
nlsy <- read_csv("nlsy_cc.csv")
colnames(nlsy) <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
                    "id", "nsibs", "samp", "race_eth", "sex", "region", 
                    "income", "res_1980", "res_2002", "age_bir")

#### functions -----------------------------------------------------

# Look inside a function
CreateTableOne
# Look inside another function you know!

# make a practice vector for our new_mean function
x <- c(1, 3, 5, 7, 9)

# try out our code
n <- length(x)
sum(x) / n

# insert into the function body
new_mean <- function(x) {
  n <- length(x)
  mean_val <- sum(x) / n
  return(mean_val)
}

# try it out!
new_mean(x = x)
new_mean(x = c(100, 200, 300))
# try it with another vector! How about nlsy$income?

## proportions ##

# new test vector
x <- c(0, 1, 1)

# two options:
percent <- 100
percent * sum(x) / length(x)
percent <- 1
percent * sum(x) / length(x)

# add percent as an argument, with a default of 1
prop <- function(x, percent = 1) {
  n <- length(x)
  mean_val <- percent * sum(x) / n
  return(mean_val)
}

prop(x = x)
prop(x = x, percent = 100)

#### Exercises 1 -----------------------------------------------------

# 1. You're tired of writing x^2 when you want to square x. Make a function to
# square a number. You can call it square().

# 2. You don't just want to square numbers, you want to raise them to higher
# powers too. Make a function that uses two arguments, x for a number, and
# power for the power. Call it raise().

# 3. Change your raise() function to default to squaring x when the user doesn't
# enter a value for power.

# 4. Use your function to square and cube 524 with raise(524) and raise(524, power
# = 3).

#### functions 2 -----------------------------------------------------

# we want to be able to change the quantile we want:
q <- .5
nlsy %>% summarize(
  q_var = quantile(age_bir, probs = q)
)

# paste in the body of a function
age_bir_q <- function(q) {
  quant <- nlsy %>% 
    summarize(q_var = quantile(age_bir, probs = q))
  return(quant)
}

# test
age_bir_q(q = 0.5)
median(nlsy$age_bir)

## what if we want to change the variable? ##
# not going to work:
var_q <- function(q, var) {
  quant <- nlsy %>%
    summarize(q_var = quantile(var, probs = q))
  return(quant)
}
var_q(q = 0.5, var = income)
var_q(q = 0.5, var = "income")

# we can accept variable names as quotes with rename
# compare:
nlsy
nlsy %>% rename(eyeglasses = "glasses")

# if we rename the variable we pass as an argument,
# our function will work
var_q <- function(q, var) {
  quant <- nlsy %>%
    rename(new_var = var) %>%
    summarise(q_var = quantile(new_var, probs = q))
  return(quant)
}
var_q(q = 0.5, var = "income")
var_q(q = 0.25, var = "sleep_wkdy")
var_q(q = 0.95, var = "nsibs")

# what if we want to change the grouping variable?
# i.e., from sex to race_eth here:
nlsy %>% group_by(sex) %>% summarise(mean_inc = mean(income))
nlsy %>% group_by(race_eth) %>% summarise(mean_inc = mean(income))

#### Exercises 2 -----------------------------------------------------

# 1. Write a function to calculate the stratified mean income for grouping
# variable var. In other words, write a function such that mean_group_inc(var =
# "sex") produces the same results as the first line on the previous slide,
# mean_group_inc(var = "race_eth") the second.

# 2. Rewrite your function to accept two arguments: group_var to determine what
# the grouping variable is, and mean_var to determine what variable you want to
# take the mean of (e.g., mean_group(group_var = "sex", mean_var = "income")
# should give you the same results as above).


#### for loops -----------------------------------------------------

# just print the values
for (i in 1:3) {
  print(i)
}

# have to use the print function to print to the console
# try taking out the print() here:
qs <- c(0.1, 0.5, 0.9)
for (i in qs) {
  print(var_q(q = i, var = "income"))
}

# create an empty object to store results
results <- rep(NA, 3)
results
for (i in 1:3) {
  results[[i]] <- i * 1.5
}
results

# revise our function: use pull() to get just one variable, not return a tibble
var_q_new <- function(q, var) {
  quant <- nlsy %>%
    rename(new_var = var) %>%
    summarise(q_var = quantile(new_var, probs = q)) %>%
    pull(q_var)
  return(quant)
}
var_q_new(q = 0.5, var = "income")

# set up the loop
qs <- seq(0.1, 0.9, by = 0.1)
qs
deciles <- rep(NA, length(qs))
deciles
seq_along(qs)

# run the loop
for (i in seq_along(qs)) {
  deciles[[i]] <- var_q_new(q = qs[[i]], 
                            var = "income")
}
deciles

#### Exercises 3 -----------------------------------------------------

# 1. Change the last for loop in the slides to loop over different variables
# instead of different quantiles. That is, calculate the 0.25 quantile for each
# of c("income", "age_bir", "nsibs") in a for loop.

# 2. You can nest for loops inside each other, as long as you use different
# iteration variables. Write a nested for loop to iterate over variables (with
# i) and quantiles (with j). You'll need to start with an empty matrix instead
# of a vector, with rows indexed by i and columns by j. Calculate each of the
# deciles for each of the above variables.


#### scoped functions -----------------------------------------------------

# just to explore
nlsy %>%
  summarise_at(vars(contains("sleep")), 
               list(med = median, sd = sd))

nlsy %>%
  summarise_if(is.numeric, 
               mean)

nlsy %>%
  mutate_at(vars(eyesight, race_eth, sex),
            factor) %>%
  select(eyesight, race_eth, sex)

nlsy %>%
  rename_all(toupper)

#### Challenge -----------------------------------------------------

# Create a function that calculates the stratified proportion of people with
# different levels of eyesight by any categorical variable. Then use any
# technique (besides copying and pasting) to calculate the proportions
# stratified by sex, race_eth, and region. You should end up with something
# like this:

## # A tibble: 45 x 5
##    var      var_level eyesight     n    prop
##    <chr>        <dbl>    <dbl> <int>   <dbl>
##  1 sex              1        1   228   0.455  
##  2 sex              1        2   162   0.323  
##  3 sex              1        3    85   0.170  
##  4 sex              1        4    21   0.0419 
##  5 sex              1        5     5   0.00998
##  6 sex              2        1   246   0.349  
##  7 sex              2        2   223   0.317  
##  8 sex              2        3   164   0.233  
##  9 sex              2        4    57   0.0810 
## 10 sex              2        5    14   0.0199 
## 11 race_eth         1        1    90   0.427  
## 12 race_eth         1        2    54   0.256  
## 13 race_eth         1        3    50   0.237  
## 14 race_eth         1        4    14   0.0664 
## 15 race_eth         1        5     3   0.0142 
## 16 race_eth         2        1   102   0.332  
## 17 race_eth         2        2    96   0.313  
## 18 race_eth         2        3    73   0.238  
## 19 race_eth         2        4    29   0.0945 
## 20 race_eth         2        5     7   0.0228 
## 21 race_eth         3        1   282   0.410  
## 22 race_eth         3        2   235   0.342  
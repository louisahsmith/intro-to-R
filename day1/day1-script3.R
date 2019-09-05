#====================================================================#
# Title: Day 1, script 3                                             #
# Name:  Your name here                                              #
#====================================================================#

#### Code ------------------------------------------------------------
# Run through this code on your own to practice what we covered in
# the slides.

# Before you do anything, restart R by going to Session -> Restart R
# or using the keyboard shortcut shift-cmd/ctrl-F10.
# This won't delete any of your work, even if you haven't saved your script,
# but it will unload all the packages and remove all objects from your environment.
# It's best practice to start fresh every time.

# since we restarted R, we need to reload packages and data
library(tidyverse)
nlsy <- read_csv("nlsy_cc.csv")
colnames(nlsy) <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
                    "id", "nsibs", "samp", "race_eth", "sex", "region", 
                    "income", "res_1980", "res_2002", "age_bir")

# explore the data
glimpse(nlsy)
summary(nlsy)

# we can refer to specific variables with $ notation
summary(nlsy$glasses)
mean(nlsy$age_bir)
# what happens if you just run nlsy$eyesight?
# what about table(nlsy$eyesight)

# starting plot:
ggplot(data = nlsy) +
  geom_point(aes(x = income, y = age_bir))

# ending plot:
ggplot(data = nlsy) +
  geom_point(aes(x = income, y = age_bir,
                 color = factor(eyesight))) +
  scale_color_brewer(palette = "Set1",
                     name = "Eyesight",
                     labels = c("Excellent",
                                "Very Good",
                                "Good",
                                "Fair",
                                "Poor"))

# add and subtract to these two plots to see what happens!

#### Exercises 3 -----------------------------------------------------
# Answer the questions below. Remember to include your answers in
# this script so you can save and look back at it!

# 1. Using the NLSY data, make a scatter plot of the relationship between hours
# of sleep on weekends and weekdays. Color it according to region (where 1 =
# northeast, 2 = north central, 3 = south, and 4 = west).
# Answer:

# 2. Replace `geom_point()` with `geom_jitter()`. What does this do? Why might
# this be a good choice for this graph? Play with the `width = ` and `height =
# ` options. This site may help:
# https://ggplot2.tidyverse.org/reference/geom_jitter.html
# Answer:

# 3. Use the `shape = ` argument to map the sex variable to different shapes.
# Change the shapes to squares and diamonds. (Hint: how did we manually change
# colors to certain values? This page might also help:
# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html)
# Answer:

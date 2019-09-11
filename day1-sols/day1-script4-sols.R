#====================================================================#
# Title: Day 1, script 4                                             #
# Name:  Your name here                                              #
#====================================================================#

#### Code ------------------------------------------------------------
# Run through this code on your own to practice what we covered in
# the slides.

# Restart R again! Then copy and paste what you need at the start of this script
library(tidyverse)
nlsy <- read_csv("nlsy_cc.csv")
colnames(nlsy) <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
                    "id", "nsibs", "samp", "race_eth", "sex", "region", 
                    "income", "res_1980", "res_2002", "age_bir")

# make a faceted plot
# play with these graphs
# here we have a cols argument, what if we change to rows? what if we have both?
ggplot(data = nlsy) +
  geom_bar(aes(x = nsibs)) +
  labs(x = "Number of siblings") +
  facet_grid(cols = vars(region),
             margins = TRUE,
             scales = "free_y")


ggplot(data = nlsy) +
  geom_bar(aes(x = nsibs)) +
  labs(x = "Number of siblings") +
  facet_wrap(vars(region),
             ncol = 3)

# make a histogram
ggplot(data = nlsy) +
  geom_histogram(aes(x = income),
                 bins = 100) +
  scale_x_log10()

#### Exercises 4 -----------------------------------------------------
# Answer the questions below. Remember to include your answers in
# this script so you can save and look back at it!

# 1. When we're comparing distributions with very different numbers of
# observations, instead of scaling the y-axis like we did with the
# `facet_grid()` function, we might want to make density histograms. Use google
# to figure out how to make a density histogram of income. Facet it by region.
ggplot(data = nlsy) +
  geom_histogram(aes(x = income, y = ..density..),
                 bins = 100) +
  scale_x_log10() +
  facet_grid(rows = vars(region))

# 2. Make each of the regions in your histogram from part 1 a different color.
# (Hint: compare what `col = ` and `fill = ` do to histograms).
ggplot(data = nlsy) +
  geom_histogram(aes(x = income, y = ..density.., fill = factor(region)),
                 bins = 100) +
  scale_x_log10() +
  facet_grid(rows = vars(region))

# 3. Instead of a log-transformed x-axis, make a square-root transformed x-axis.
ggplot(data = nlsy) +
  geom_histogram(aes(x = income, y = ..density.., fill = factor(region)),
                 bins = 100) +
  scale_x_sqrt() +
  facet_grid(rows = vars(region))


# 4. Doing part 3 squishes the labels on the x-axis. Using the `breaks = `
# argument that all the `scale_x_()` functions have, make labels at 1000,
# 10000, 25000, and 50000.
ggplot(data = nlsy) +
  geom_histogram(aes(x = income, y = ..density.., fill = factor(region)),
                 bins = 100) +
  scale_x_sqrt(breaks = c(1000, 10000, 25000, 50000)) +
  facet_grid(rows = vars(region))

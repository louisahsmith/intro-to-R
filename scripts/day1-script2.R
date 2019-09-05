#====================================================================#
# Title: Day 1, script 2                                             #
# Name:  Your name here                                              #
#====================================================================#

#### Code ------------------------------------------------------------
# Run through this code on your own to practice what we covered in
# the slides.

# install.packages("tidyverse") # you would uncomment and run this line
                                # on your own computer if you have never 
                                # installed this package before
# load the package
library(tidyverse) # you have to run this line every time you run the script

# most of the time you'll read in your own data, but here are two ways to 
# create it in R
dat1 <- tibble(
  age = c(24, 76, 38),
  height_in = c(70, 64, 68),
  height_cm = height_in * 2.54
)
dat1

dat2 <- tribble(
  ~n, ~food, ~animal,
  39, "banana", "monkey",
  21, "milk", "cat",
  18, "bone", "dog"
)
dat2

# read in data
# there are different functions to read in data depending on what kind it is
# you can use the haven package to read in SAS and Stata files
nlsy <- read_csv("nlsy.csv") # assumes the file is in the same folder as this project
nlsy


# look at the ugly column names
# where can you find this info in your environment window?
colnames(nlsy)
# "write over" the variable names with new names
colnames(nlsy) <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
                    "id", "nsibs", "samp", "race_eth", "sex", "region", 
                    "income", "res_1980", "res_2002", "age_bir")
# look at the column names again to confirm they changed
colnames(nlsy)


#### Exercises 2 -----------------------------------------------------
# Answer the questions below. Remember to include your answers in
# this script so you can save and look back at it!

# 1. How many people are in the NLSY? How many variables are in this dataset?
# What are two ways you can answer these questions? 
# Answer:

# 2. Can you find an R function(s) we haven't discussed that answers question 2? 
# (Hint: Google) 
# Answer:

# 3. I've also provided you with the same dataset as an Excel document, but it's
# not on the first sheet, and there's an annoying header. Load the `readxl`
# package (you already installed with with `tidyverse`, but it doesn't load
# automatically). Figure out how to read in the data. This may help:
# https://readxl.tidyverse.org.
# Answer: 

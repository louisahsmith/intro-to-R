#====================================================================#
# Title: Day 1, script 1                                             #
# Name:  Your name here                                              #
#====================================================================#

#### Code ------------------------------------------------------------
# Run through this code on your own to practice what we covered in
# the slides.

# create a vector of numeric values
vals <- c(1, 645, 329)
vals

# run these lines of code one at a time and compare what each does
# what happens in your environment window? what about your console?
new_vals
c(13, 7245, 23, 49.32)
new_vals <- c(13, 7245, 23, 49.32)
new_vals

# create and view different types of vectors
chars <- c("dog", "cat", "rhino")
chars
logs <- c(TRUE, FALSE, FALSE)
logs

# create a matrix
mat <- matrix(c(234, 7456, 12, 654, 183, 753), nrow = 2)
mat

# pull out rows
mat[2, ]

#### Exercises 1 -----------------------------------------------------
# Answer the questions below. Remember to include your answers in
# this script so you can save and look back at it!

# 1. Extract `645` from `vals` using square brackets
# Answer:

# 2. Extract `"rhino"` from `chars` using square brackets
# Answer:

# 3. You saw how to extract the second row of `mat`. Figure out how to extract the second column.
# Answer:

# 4. Extract `183` from `mat` using square brackets
# Answer:

# 5. Figure out how to get the following errors: 
#      incorrect number of dimensions
#      subscript out of bounds
# Answer:






## Exercise 1

# install.packages("tidyverse") # you would uncomment and run this line
# on your own computer if you have never installed this package before
library(tidyverse) # you have to run this line every time 

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

nlsy <- read_csv("nlsy.csv")
nlsy
colnames(nlsy)
colnames(nlsy) <- c("id", "samp", "sex", "black", "eyesight", "glasses", "income", "age_bir")

# Exercise 2
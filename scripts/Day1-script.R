
# create values
vals <- c(1, 645, 329)

vals

chars <- c("dog", "cat", "rhino")
chars

logs <- c(TRUE, FALSE, FALSE)
logs

mat <- matrix(c(234, 7456, 12, 654, 183, 753), nrow = 2)
mat

mat[2, ]

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
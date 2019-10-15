library(tidyverse)
colnames_nlsy <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
                    "id", "nsibs", "samp", "race_eth", "sex", "region", 
                    "income", "res_1980", "res_2002", "age_bir")

#### ORGANIZATION ####

## ---- here examples -------------------------------------------------------
## source(here::here("code", "functions.R"))
## 
## dat <- read_csv(here::here("data", "raw", "data.csv"))
## 
## p <- ggplot(dat) + geom_point(aes(x, y))
## 
## ggsave(plot = p,
##        filename = here::here("results", "figures", "fig.pdf"))
##        

# check to see where YOU are
# install.packages(here) # if haven't already
here::here()

# this would give you the same result
## library(here)
## here()


## ---- source examples ---------------------------------------------------
## source("script.R")
## source(here::here("code", "functions.R"))

# try this
# go to this URL (or look back at Day 1's code) to see where the error is
# did it create any new objects in your environment?
source("https://raw.githubusercontent.com/louisahsmith/intro-to-R/master/day1/day1-script1.R")


## ---- reading in data examples --------------------------------------------
## # the readr functions are loaded with library(tidyverse)
## dat <- readr::read_csv("data.csv")
## dat <- readr::read_table("data.dat")
## # saved as an R object with write_rds()
## dat <- readr::read_rds("data.rds")
## dat <- readxl::read_excel("data.xlsx")
## dat <- haven::read_sas("data.sas7bdat")
## dat <- haven::read_stata("data.dta")


#### MISSING DATA ####

## ------------------------------------------------------------------------
NA > 3
mean(c(1, 2, NA))
mean(c(1, 2, NA), na.rm = TRUE)


## ------------------------------------------------------------------------
vals <- c(1, 2, NA)
is.na(vals)
anyNA(vals)
na.omit(vals)


## read in the data
## the skip = 1 tells it to skip the column names, because we're providing them
nlsy <- read_csv("nlsy.csv", col_names = colnames_nlsy, skip = 1)

## --- compare ----------------------------------------------------------
nlsy[1, c("id", "glasses", "age_bir")]
nlsy_na <- nlsy %>% na_if(-1) %>% na_if(-2) %>% 
  na_if(-3) %>% na_if(-4) %>% na_if(-5)
nlsy_na[1, c("id", "glasses", "age_bir")]


## --- "get rid of" the first person by making his ID missing --------------
nlsy_bad <- nlsy %>% 
  mutate(id = na_if(id, 1))
nlsy_bad[1:2, c("id", "glasses", "age_bir")]


## ---can specify values like this ---------------------------------------
nlsy <- read_csv("nlsy.csv", 
          col_names = colnames_nlsy, skip = 1,
          na = c("-1", "-2", "-3", "-4", "-5"))


## --- for complete case analysis -------------------------------
nrow(nlsy)
nlsy_cc <- nlsy %>% filter(complete.cases(nlsy))
nrow(nlsy_cc)
nlsy2 <- nlsy %>% select(id, glasses, eyesight) %>% na.omit()
nrow(nlsy2)

#### SHARING RESULTS ####

## ---- preparing data for analysis --------------------------------------------
nlsy_clean <- nlsy %>%
  mutate(
    region = factor(region, labels = c("Northeast", "North central", "South", "West")),
    sex = factor(sex, labels = c("Male", "Female")),
    log_inc = ifelse(income > 0, log(income), NA),
    eyesight = factor(eyesight, labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    race_eth = factor(race_eth, labels = c("Hispanic", "Non-Hispanic Black", "Non-Black, Non-Hispanic"))
  )

## -----------------------------------------------------------
## # load packages
## library(tidyverse) (did above)
## # must install if haven't already
library(broom) # for making pretty model output
library(splines) # for adding splines
## 
## # read in data (created above instead)
## nlsy_clean <- read_rds(here::here("data", "nlsy_clean.rds"))


## ---- types of regression ---------------------------------------------
# linear regression (OLS)
mod_lin1 <- lm(log_inc ~ age_bir + sex + race_eth,
                 data = nlsy_clean)
# another way to do linear regression (GLM)
mod_lin2 <- glm(log_inc ~ age_bir + sex + race_eth,
                family = gaussian(link = "identity"),
                data = nlsy_clean)
# logistic regression
mod_log <- glm(glasses ~ eyesight + sex + race_eth,
               family = binomial(link = "logit"),
               data = nlsy_clean)
# poisson regression
mod_pois <- glm(nsibs ~ sleep_wkdy + sleep_wknd,
                  family = poisson(link = "log"),
                  data = nlsy_clean)


## ---- extra regression features -----------------------------------------
mod_big <- glm(log_inc ~ sex * age_bir +
                 nsibs + I(nsibs^2) +
                 ns(sleep_wkdy, knots = 3),
          family = gaussian(link = "identity"),
          data = nlsy_clean)


## ---- look back at first logistic model -----------------------------------
summary(mod_log)


## ------------------------------------------------------------------------
# from the broom package
tidy(mod_log)


## ------------------------------------------------------------------------
coef(mod_log)
coef(mod_log)[6]
tidy(mod_log) %>% slice(6) %>% pull(estimate)


## ------------------------------------------------------------------------
res_mod_log <- mod_log %>% tidy() %>% 
  mutate(lci = estimate - 1.96 * std.error,
         uci = estimate + 1.96 * std.error)
res_mod_log

## ------------------------------------------------------------------------
res_mod_log <- res_mod_log %>% select(term, estimate, lci, uci) %>%
  filter(term != "(Intercept)") %>%
  mutate_at(vars(estimate, lci, uci), exp)
res_mod_log


## ------------------------------------------------------------------------
res_mod_log %>% select(term, estimate, lci, uci) %>%
  filter(term != "(Intercept)") %>%
  mutate(ci = str_glue("({lci}, {uci})"))



## ------------------------------------------------------------------------
ci_func <- function(estimate, lci, uci) {
  OR <- round(exp(estimate), 2)
  lci <- round(exp(lci), 2)
  uci <- round(exp(uci), 2)
  to_print <- str_glue("{OR} (95% CI {lci}, {uci})")
  return(to_print)
}


## ---- test ---------------------------------------------------------------
ci_func(.2523421, -.142433, .851234)


## ------------------------------------------------------------------------
new_mod <- glm(glasses ~ eyesight*sex, family = binomial(link = "logit"),
               data = nlsy_clean)
new_mod %>% tidy() %>%
  filter(term != "(Intercept)") %>%
  mutate(lci = estimate - 1.96 * std.error,
         uci = estimate + 1.96 * std.error,
         OR = ci_func(estimate, lci, uci),
         p.value = scales::pvalue(p.value)) %>%
  select(term, OR, p.value)


#### CHALLENGE ####

# some helpful code hints 

## -----------------------------------------------------------
## tab1 <- CreateTableOne(...) %>% print() %>% as_tibble(rownames = "id")
## write_csv(tab1, ...)


## -----------------------------------------------------------
## ggplot(data) +
##   geom_point(aes(x = , y = )) +
##   geom_errorbar(aes(x = , ymin = , ymax = ))


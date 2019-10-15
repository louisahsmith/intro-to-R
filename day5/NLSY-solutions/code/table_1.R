library(tableone)
library(tidyverse)

nlsy <- read_rds(here::here("data", "processed", "nlsy_clean.rds"))

tab1 <- CreateTableOne(
  data = nlsy,
  vars = c("eyesight", "nsibs", "race_eth",
           "sex", "region", "income", "age_bir"),
  strata = "sex",
  factorVars = c("eyesight", "race_eth", "sex", "region")
) %>% 
  print() %>% 
  as_tibble(rownames = "id")

write_csv(tab1, here::here("results", "tables", "table_1.csv"))

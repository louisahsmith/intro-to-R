library(tidyverse)
library(broom)

source(here::here("code", "functions.R"))

nlsy <- read_rds(here::here("data", "processed", "nlsy_clean.rds"))

mod <- lm(log_inc ~ age_bir + sex + race_eth + nsibs, data = nlsy)

res <- tidy(mod) %>%
  mutate(
    lci = estimate - 1.96 * std.error,
    uci = estimate + 1.96 * std.error,
    ci = ci_func(estimate, lci, uci, digits = 3),
    p = scales::pvalue(p.value),
    term = fct_recode(term,
      "age at 1st birth" = "age_bir",
      "female (vs. male)" = "sexFemale",
      "non-Hisp. Black (vs. Hisp.)" = "race_ethNon-Hispanic Black",
      "non-Black, non-Hisp. (vs. Hisp.)" = "race_ethNon-Black, Non-Hispanic",
      "number of siblings" = "nsibs"
    )) %>%
  filter(term != "(Intercept)")

tab_res <- res %>%
  select(term, ci, p)

write_csv(tab_res, here::here("results", "tables", "lin_reg_tab.csv"))

ggplot(res) +
  geom_point(aes(term, estimate)) +
  geom_errorbar(aes(x = term, ymin = lci, ymax = uci)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust = 1)) +
  labs(x = NULL, y = expression(hat(beta)),
       title = "Coefficient estimates from linear regression\nof log(income) on various factors")

ggsave(here::here("results", "figures", "lin_reg_fig.pdf"))

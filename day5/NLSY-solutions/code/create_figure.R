library(tidyverse)

nlsy <- read_rds(here::here("data", "processed", "nlsy_clean.rds"))

ggplot(nlsy, aes(nsibs, age_bir, col = factor(region))) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Relationship between family size and age at first birth",
       subtitle = "By U.S. Region, with Loess curves",
       x = "Number of siblings",
       y = "Age at first birth") +
  scale_color_viridis_d(name = "Region") +
  facet_grid(cols = vars(sex)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(here::here("results", "figures", "fam_size_age.png"))

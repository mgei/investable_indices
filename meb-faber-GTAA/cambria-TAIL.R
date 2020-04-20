library(tidyverse)
library(tidyquant)

data <- tq_get(c("SPY", "TAIL"))

data %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted, mutate_fun = periodReturn,
               period = "weekly", type = "log") %>% 
  pivot_wider(names_from = symbol, values_from = weekly.returns) %>% 
  filter(!is.na(TAIL)) %>% 
  ggplot(aes(x = SPY, y = TAIL)) +
  geom_point() +
  geom_smooth()

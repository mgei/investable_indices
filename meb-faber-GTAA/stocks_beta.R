library(tidyquant)
library(tidyverse)

set.seed(1)

random_stocks <- tq_exchange("NASDAQ") %>% 
  sample_n(10)

data_stocks <- tq_get(random_stocks)

min_date <- data_stocks %>% 
  group_by(symbol) %>% 
  summarise(min_date = min(date)) %>% 
  summarise(min_date = max(min_date)) %>% 
  pull()

min_date

returns_stocks <- data_stocks %>% 
  filter(date >= min_date) %>% 
  group_by(symbol, company) %>% 
  mutate(Ra = log(adjusted/lag(adjusted)))

data_index <- tq_get("^IXIC", from = min_date)

returns_index <- data_index %>%
  filter(date >= min_date) %>% 
  mutate(Rb = log(adjusted/lag(adjusted)))

returns_all <- returns_stocks %>% 
  select(date, symbol, company, Ra) %>% 
  left_join(return_index %>% select(date, Rb), by = "date")

betas <- returns_all %>%
  group_by(symbol, company) %>% 
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM) %>% 
  select(symbol, company, Beta)

betas

returns_all %>% 
  group_by(symbol, company) %>% 
  ggplot(aes(x = Rb, y = Ra)) + 
  geom_smooth() +
  geom_point(size = 0.3) + 
  facet_wrap(~symbol)
  

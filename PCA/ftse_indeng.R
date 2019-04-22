library(tidyverse)
library(tidyquant)
library(lubridate)
library(quantmod)

## Data ----

# index
# https://www.investing.com/indices/industrial-engineering-historical-data
index <- read_csv("FTSE 350 Industrial Engineering Historical Data.csv")

index %<>%
  rename(date = Date) %>% 
  mutate(date = mdy(date),
         adjusted = Price) %>% 
  select(-Vol., -`Change %`)

# The index components are Bodycote, Hill&Smith, IMI, Rotork, Spirax-Sarco Engineering, and Weir Group.
# https://www.investing.com/indices/industrial-engineering-components

bodycote   <- tq_get("BOY.L")
hill_smith <- tq_get("HILS.L")
imi        <- tq_get("IMI.L")
rotork     <- tq_get("ROR.L")
spirax_s   <- tq_get("SPX.L")
weir       <- tq_get("WEIR.L")

data <- bind_rows(index %>%      mutate(type = "index", name = "FTSE 350 Industrial Engineering"), 
                  bodycote %>%   mutate(type = "stock", name = "Bodycote plc"), 
                  hill_smith %>% mutate(type = "stock", name = "Hill & Smith Holdings PLC"), 
                  imi %>%        mutate(type = "stock", name = "IMI plc"), 
                  rotork %>%     mutate(type = "stock", name = "Rotork plc"), 
                  spirax_s %>%   mutate(type = "stock", name = "Spirax-Sarco Engineering plc"), 
                  weir %>%       mutate(type = "stock", name = "The Weir Group PLC"))

# compute returns (daily)
data_rets <- data %>% 
  select(date, name, type, adjusted) %>% 
  arrange(type, name, date) %>% 
  group_by(name, type) %>%
  mutate(return = adjusted/lag(adjusted)-1,
         return_ln = log(adjusted/lag(adjusted))) %>% 
  filter(!is.na(return_ln),
         # cut-off date
         date >= "2016-04-15") %>% 
  mutate(cumret_ln = cumsum(return_ln),
         adjusted_ln = exp(cumret_ln))

data_rets %>% 
  ggplot(aes(x = date, y = adjusted_ln, color = name)) +
  geom_line(aes(size = factor(desc(type)))) +
  scale_size_discrete(range = c(0.4,1.4), guide = "none") +
  theme_tq()

# compute returns (monthly)
data_rets_m <- data %>% 
  select(date, name, type, adjusted) %>% 
  arrange(type, name, date) %>% 
  group_by(name, type, date = floor_date(date, unit = "months") %>% date()) %>%
  summarise(adjusted = first(adjusted)) %>% 
  mutate(return = adjusted/lag(adjusted)-1,
         return_ln = log(adjusted/lag(adjusted))) %>% 
  filter(!is.na(return_ln),
         # cut-off date
         date >= "2016-04-01") %>% 
  mutate(cumret_ln = cumsum(return_ln),
         adjusted_ln = exp(cumret_ln))

data_rets_m %>% 
  ggplot(aes(x = date, y = adjusted_ln, color = name)) +
  geom_line(aes(size = factor(desc(type)))) +
  scale_size_discrete(range = c(0.4,1.4), guide = "none") +
  theme_tq()

# replicating the weights
data_rets %>%
  ungroup() %>% 
  select(date, name, return_ln) %>% 
  filter(date >= "2016-04-15") %>% 
  spread(name, return_ln) -> returns_daily

lm(`FTSE 350 Industrial Engineering` ~ . -1, data = returns_daily %>% select(-date)) -> model

library(broom)
tidy(model)

# The "estimate" can be seen as weights, shoud approximately sum up to 1
# The remaining is tracking error, other or unexplained factors 
tidy(model) %>% 
  summarise(total = sum(estimate))

# note R-square of .99
summary(model)

# stats
data_rets %>% 
  ungroup() %>% 
  select(date, name, return_ln) %>% 
  spread(name, return_ln) %>% 
  select(-date) %>% 
  chart.Correlation()

data_rets_m %>% 
  ungroup() %>% 
  select(date, name, return_ln) %>% 
  spread(name, return_ln) %>% 
  select(-date) %>% 
  chart.Correlation()

# PCA on monthly log returns
data_rets_m %>%
  filter(type != "index") %>% 
  ungroup() %>% 
  select(date, name, return_ln) %>% 
  # fill to remove the misssing values
  spread(name, return_ln, fill = 0) %>% 
  select(-date) %>% 
  as.matrix() %>% 
  prcomp(scale. = T, center = T) -> pca

pca
pca %>% summary()

library(factoextra)
pca %>% fviz_eig()
pca %>% fviz_pca_biplot()

# PCA loadings (eigenvectors)
pca$rotation

# eigenvalues
pca$sdev^2

# varimax rotation
pca$rotation[,1:3] %>% varimax()

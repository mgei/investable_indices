library(tidyverse)
library(readxl)
library(lubridate)

raw <- read_excel("snb-data-bawebedomsecwa-de-all-20200522_0900.xlsx", skip = 24, col_names = F)

classes <- raw %>% 
  head(n=5)


data <- raw %>% 
  tail(nrow(raw)-5)

data_long <- data %>% 
  rename(date = 1) %>% 
  mutate(date = ymd(paste0(date, "-01"))) %>% 
  pivot_longer(cols = -date) %>% 
  arrange(name, date) %>% 
  left_join(classes[1,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[2,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[3,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[4,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[5,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(symbol = value) %>% 
              select(-1),
            by = "name") %>% 
  select(-name) %>% 
  mutate(value = as.double(value))

data_long_clean <- data_long %>% 
  filter(!is.na(value))

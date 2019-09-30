library(tidyverse)
library(tidyquant)
library(magrittr)

# load data we have in csv form ----
directory <- list.files("pricedata/.")

# empty tibble for prices, needs the date column
pricedata <- tibble(date = as.Date("1900-01-01")) %>% 
  filter(date != as.Date("1900-01-01"))

for (file in directory) {
  print(paste(which(directory == file), file))
  # make it monthly
  temp <- read_csv(paste0("pricedata/", file), col_types = "Ddddddd") %>% 
    tq_transmute(select     = open:adjusted, 
                 mutate_fun = to.period, 
                 period     = "months") %>% 
    mutate(date = ceiling_date(date, unit = "months") - days(1)) %>% 
    select(date, !!str_remove(file, ".csv") := adjusted)
  
  pricedata %<>% full_join(temp, by = "date")
  rm(temp)
  
  print("done")
}

pricedata %>% 
  arrange(date) %>%
  gather(symbol, price, -date) %>% 
  filter(!is.na(price), !is.na(symbol)) %>% 
  arrange(symbol, date) %>%  
  group_by(symbol) %>% 
  fill(price, .direction = "down") %>% 
  tq_transmute(select     = price, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               type       = "arithmetic") -> wrets

wrets %>% 
  mutate(launch = min(date), launchdbl = as.double(launch), launchsymbol = str_c(launchdbl, symbol)) %>% 
  arrange(launchsymbol) -> temp

temp %>% 
  select(symbol) %>% 
  distinct() %>% 
  pull() -> symbol_colnames

temp %>% 
  ungroup() %>%
  select(-launch, -launchdbl, -symbol) %>% 
  spread(launchsymbol, weekly.returns) -> temp

colnames(temp) <- c("date", symbol_colnames)

temp %>% View()

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
                 period     = "weeks") %>% 
    mutate(date = ceiling_date(date, unit = "weeks") - days(1)) %>% 
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
  spread(launchsymbol, weekly.returns) %>% 
  fill(2:ncol(.), .direction = "down") -> temp

colnames(temp) <- c("date", symbol_colnames)

temp %>% 
  filter(row_number() != 1) -> wrets_wide

wrets_wide %>% 
  mutate(available = rowSums(!is.na(.))-1) %>% 
  select(available, everything()) %>% 
  filter(available >= 500) %>% 
  select(2:(.$available[1]+2)) -> yolo

yolo %>% View()

yolo %>% 
  select(-date) %>% 
  as.matrix() %>% 
  prcomp(scale. = T, center = T)


## simulate ----

simulate_performance <- function(returns, xdate, wback, wforth) {
  
  if (class(xdate) != "Date") {
    xdate <- as.Date(xdate)
  }
  
  xdate <- returns %>% 
    ungroup() %>% 
    select(date) %>% 
    distinct() %>% 
    arrange(date) %>% 
    mutate(dat = abs(date - xdate)) %>%
    filter(dat == abs(min(dat))) %>% 
    select(date) %>% 
    first() %>% 
    pull()
  
  xminus <- xdate - weeks(wback)
  xplus  <- xdate + weeks(wforth)  
  
  training <- wrets %>% 
    group_by(symbol) %>% 
    filter(first(date) <= xminus, last(date) >= xplus) %>% 
    ungroup() %>% 
    filter(date >= xminus, date <= xdate) %>% 
    # the following is to avoid the symbols that have a gap in the return history (we need returna for all weeks)
    group_by(symbol) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(n == max(n)) %>% 
    select(-n)
  
  training_matrix <- training %>% 
    spread(symbol, weekly.returns, fill = 0) %>% 
    select(-date) %>% 
    as.data.frame() %>% 
    as.matrix()
  
  # training_matrix %>% View()
  
  training_matrix %>% prcomp(scale. = T, center = T) %>% .$rotation %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "symbol") %>% 
    as_tibble() -> pca_loadings
  
  pca_loadings %>%   
    mutate_if(is.numeric, abs) %>% 
    filter(symbol %in% c("SPY", "GLD", "AGG", "IVV"))
  
  pca_loadings %>% 
    mutate_if(is.numeric, abs) %>%
    arrange(desc(PC1))
  
  training_matrix[,c(1,2,223,224,225)] %>% View()
  
  # FVC 
  wrets %>% 
    ungroup() %>% 
    filter(symbol == "FVC") %>% View()
    ggplot(aes(x = date, y = weekly.returns)) +
    geom_col()
    
    group_by(symbol) %>% 
    summarise(f = first(date),
              l = last(date))
    group_by(symbol) %>% 
  }
  
  
  
}

returns <- wrets

xdate <- as.Date("2016-01-02")




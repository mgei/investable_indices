library(tidyverse)
library(tidyquant)
library(purrr)

source("../shiny-comparison/setup.R")

max_na <- function(...) {
  out <- suppressWarnings(max(...))
  
  
  if (is.na(out)) {
    return(out)
  }
  
  if (out == -Inf | out == Inf) {
    return(NA)
  } else {
    return(out)
  }
}


fundlist <- reload_fundlist()

fundlist_prices_dividends <- fundlist %>% 
  rowwise() %>% 
  mutate(prices = list(get_six_prices_cache(ISIN = ISIN, currency = `Trading currency`, 
                                            reload_if_older_than = "1 month", cache_dir = "../shiny-comparison/data/cache_six_prices/")),
         dividends = list(get_six_dividends_cache(ISIN = ISIN, currency = `Trading currency`, 
                                                  reload_if_older_than = "1 month", cache_dir = "../shiny-comparison/data/cache_dividends/"))) %>% 
  ungroup()

fundlist_prices_dividends <- fundlist_prices_dividends %>% 
  mutate(last_dividend = map_dbl(dividends, ~.x %>% summarise(last = max_na(Ex_dividend_date)) %>% pull()) %>% 
           as.Date(),
         exists_since = map_dbl(prices, ~.x %>% summarise(first = min(Date)) %>% pull()) %>% 
           as.Date())

fundlist_prices_dividends %>% 
  saveRDS("data/fundlist_prices_dividends.RDS")

fundlist_prices_dividends %>% 
  object.size() %>% 
  format(units = "Mb")

fundlist_prices_dividends %>% 
  # mutate(prices_dividends = map2(prices, dividends, full_join))
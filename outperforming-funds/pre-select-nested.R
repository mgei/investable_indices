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
           as.Date()) %>% 
  mutate(prices = map2(prices, `Trading currency`, ~.x %>% mutate(Currency = .y)))

exchange_rates <- get_exchange_rate_cache_multi(cur1 = unique(fundlist$`Trading currency`), cur2 = "CHF")

fundlist_prices_dividends_CHF <- fundlist_prices_dividends %>% 
  mutate(prices = map(prices, ~left_join(.x, exchange_rates %>% filter(to == "CHF"), by = c("Currency" = "from", "Date" = "Date")))) %>% 
  mutate(prices = map(prices, ~.x %>% mutate(rate = na.locf0(rate),
                                             CloseCHF = Close*rate))) %>% 
  mutate(dividends = map(dividends, ~left_join(.x, exchange_rates %>% filter(to == "CHF"), by = c("Currency" = "from", "Ex_dividend_date" = "Date")))) %>% 
  mutate(dividends = map(dividends, ~.x %>% mutate(rate = na.locf0(rate),
                                                   ValueCHF = Value*rate)))

fundlist_prices_dividends_CHF_returns <- fundlist_prices_dividends_CHF_joined <- fundlist_prices_dividends_CHF %>% 
  mutate(prices_dividends = map2(prices, dividends,
                                 ~.x %>% 
                                   select(Date, CloseCHF) %>% 
                                   full_join(.y %>% select(Ex_dividend_date, ValueCHF), 
                                             by = c("Date" = "Ex_dividend_date")) %>% 
                                   arrange(Date))) %>% 
  mutate(prices_dividends = map(prices_dividends, ~.x %>% mutate(R = CloseCHF/lag(CloseCHF)-1))) %>% 
  mutate(prices_dividends = map(prices_dividends, ~.x %>% mutate(TR = (CloseCHF + replace_na(ValueCHF, 0))/lag(CloseCHF)-1)))

x <- fundlist_prices_dividends_CHF_returns %>% 
  mutate(returns_year = map(prices_dividends, ~.x %>% 
                              group_by(year = year(Date)) %>% 
                              filter(n() > 230 | year(Date) == year(Sys.Date())) %>% 
                              summarise(TR = prod(1 + TR, na.rm = T)-1)))

x %>% 
  select(Name, ISIN, `Trading currency`, returns_year) %>% 
  unnest(returns_year) %>% 
  pivot_wider(names_from = "year", values_from = "TR") %>% 
  select(Name, ISIN, `Trading currency`, as.character(1998:2020)) %>% 
  datatable()

x %>% 
  filter(ISIN == "IE00B4K6B022") %>% 
  .$prices_dividends %>% 
  tail()


fundlist_prices_dividends_CHF_joined$prices_dividends[[1]] %>% 
  mutate(TR = (CloseCHF + replace_na(ValueCHF, 0))/lag(CloseCHF)-1)





fundlist_prices_dividends_CHF_joined %>% 
  mutate(returns_year = map(prices_dividends, ~.x %>% group_by(year(Date)) %>% summarise()))

fundlist_prices_dividends %>% 
  saveRDS("data/fundlist_prices_dividends.RDS")

fundlist_prices_dividends <- readRDS("data/fundlist_prices_dividends.RDS")

fundlist_prices_dividends %>% 
  object.size() %>% 
  format(units = "Mb")

fundlist_prices_dividends %>% 
  mut

fundlist_prices_dividends %>% 
  mutate(prices_dividends = map2(prices, dividends, full_join))
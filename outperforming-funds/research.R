# 1. get price and dividend data from six for all funds
# 2. compute TRs
# 3. compute yearly performance and vola and SR (loop) and add as column to fundlist
# 4. do the same for the major indices
# 4. which one did outperform over most years and in average?

source("../shiny-comparison/setup.R")

fundlist <- readRDS("../shiny-comparison/data/fundlist.RDS")

for (i in 1:nrow(fundlist)) {
  print(i)
  get_six_prices_cache(ISIN = fundlist$ISIN[i],
                       currency = fundlist$`Trading currency`[i],
                       cache_dir = "../shiny-comparison/data/cache_six_prices/")
  
  get_six_dividends_cache(ISIN = fundlist$ISIN[i],
                          currency = fundlist$`Trading currency`[i],
                          cache_dir = "../shiny-comparison/data/cache_dividends/")
}



fundlist_SR <- tibble()
for (i in 1:nrow(fundlist)) {
  print(i)
  
  prices <- get_six_prices_cache(ISIN = fundlist$ISIN[i],
                                 currency = fundlist$`Trading currency`[i],
                                 cache_dir = "../shiny-comparison/data/cache_six_prices/") %>% 
    mutate(Currency = fundlist$`Trading currency`[i])
  
  dividends <- get_six_dividends_cache(ISIN = fundlist$ISIN[i],
                                       currency = fundlist$`Trading currency`[i],
                                       cache_dir = "../shiny-comparison/data/cache_dividends/")
  
  
  if (prices$Currency[1] != "CHF") {
    pricesCHF <- prices %>% 
      left_join(get_exchange_rate_cache(.$Currency[1], "CHF", 
                                        quandl_key = read_file("../shiny-comparison/data/quandl.key"),
                                        cache_dir = "../shiny-comparison/data/cache_exchangerates/") %>% dplyr::rename(rate = 2), 
                by = c("Date")) %>% 
      mutate(rate = na.locf(rate),
             Close = Close*rate,
             Currency = "CHF") %>% 
      select(-rate)
  } else {
    pricesCHF <- prices
  }
  
  if (nrow(dividends) > 0) {
    if (dividends$Currency[1] != "CHF") {
      dividendsCHF <- dividends %>% 
        left_join(get_exchange_rate_cache(.$Currency[1], "CHF", 
                                          quandl_key = read_file("../shiny-comparison/data/quandl.key"), 
                                          cache_dir = "../shiny-comparison/data/cache_exchangerates/") %>% 
                    dplyr::rename(rate = 2), 
                  by = c("Ex_dividend_date" = "Date")) %>% 
        arrange(Ex_dividend_date) %>% 
        mutate(rate = na.locf(rate),
               Value = Value*rate,
               Currency = "CHF") %>% 
        select(-rate)
    } else {
      dividendsCHF <- dividends
    }
  } else {
    dividendsCHF <- dividends
  }
  
  returnsCHF <- pricesCHF %>% 
    filter(Date < as.Date("2020-04-01")) %>% 
    select(-Currency) %>% 
    left_join(dividendsCHF %>% select(-ISIN, -Currency), by = c("Date" = "Ex_dividend_date")) %>% 
    mutate(CloseInclDiv = if_else(!is.na(Value), Close+Value, Close), 
           Ra = Close/lag(Close) - 1,
           RaTR = CloseInclDiv/lag(Close) - 1)
  
  stats <- returnsCHF %>% 
    group_by(year = year(Date)) %>% 
    filter(n() > 220 | year == year(Sys.Date())) %>%
    summarise(Return = prod(1+Ra)-1, 
              TotalReturn = prod(1+RaTR)-1, 
              Volatility = sd(Ra)*sqrt(250),
              SharpeRatio = TotalReturn/Volatility) %>% 
    select(year, SharpeRatio) %>% 
    pivot_wider(names_from = year, values_from = SharpeRatio)
  
  fundlist_SR <- bind_rows(fundlist_SR,
                           fundlist[i,] %>% bind_cols(stats) %>% mutate(dividends = (nrow(dividendsCHF) > 0)))
  
}


fundlist_SR %>% 
  filter(`Management style` == "active") %>% 
  select(Name, ISIN, `Trading currency`, as.character(2010:2020)) %>% arrange(desc(`2020`))

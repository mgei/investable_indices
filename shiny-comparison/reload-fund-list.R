fundlist <- readRDS("data/fundlist.RDS")

# get data from YH and SIX
for (i in 700:nrow(fundlist)) {
  
  print(i)
  
  get_prices_cache(fundlist[i, "Symbol"] %>% pull() %>% paste0(".SW"))
  get_holdings_cache(fundlist[i, "Symbol"] %>% pull() %>% paste0(".SW"))
  get_six_details_cache(fundlist[i, "ISIN"] %>% pull(),
                        fundlist[i, "Trading currency"] %>% pull())
  get_six_dividends_cache(fundlist[i, "ISIN"] %>% pull(),
                          fundlist[i, "Trading currency"] %>% pull())
}

# add info to fundlist
for (i in 1:nrow(fundlist)) {
  
  print(i)
  
  prices <- get_prices_cache(fundlist[i, "Symbol"] %>% pull() %>% paste0(".SW")) %>% 
    filter(!is.na(close))
  
  
  holdings <- get_holdings_cache(fundlist[i, "Symbol"] %>% pull() %>% paste0(".SW"))
  details <- get_six_details_cache(fundlist[i, "ISIN"] %>% pull(), 
                                   fundlist[i, "Trading currency"] %>% pull())
  dividends <- get_six_dividends_cache(fundlist[i, "ISIN"] %>% pull(), 
                                       fundlist[i, "Trading currency"] %>% pull())
  
  
  fundlist[i, "prices_nrow"] = nrow(prices)
  fundlist[i, "prices_min_date"] = suppressWarnings(min(prices$date, na.rm = T))
  fundlist[i, "prices_max_date"] = suppressWarnings(max(prices$date, na.rm = T))
  fundlist[i, "prices_currency"] = suppressWarnings(max(prices$currency, ""))

  fundlist[i, "holdings_nrow"] = nrow(holdings)
  
  fundlist[i, "details_Dividend_entitlement"] = details$Dividend_entitlement
  
  fundlist[i, "dividends_nrow"] = nrow(dividends)
  fundlist[i, "dividends_min_date"] = suppressWarnings(min(dividends$Ex_dividend_date, na.rm = T))
  fundlist[i, "dividends_max_date"] = suppressWarnings(max(dividends$Ex_dividend_date, na.rm = T))
  
}

# fundlist %>% 
#   group_by(prices_nrow > 1) %>% 
#   count()
# 
# fundlist %>% 
#   group_by(holdings_nrow >= 1) %>% 
#   count()
# 
# fundlist %>% 
#   group_by(details_Dividend_entitlement) %>% 
#   count()
# 
# fundlist %>% 
#   group_by(dividends_nrow > 1) %>% 
#   count()

fundlist %>% saveRDS("data/fundlist_plus.RDS")

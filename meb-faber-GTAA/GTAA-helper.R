get_six_prices <- function(ISIN, currency = "CHF") {
  
  url <- paste0("https://www.six-group.com/itf/fqs/delayed/charts.json?select=ISIN,ClosingPrice,ClosingPerformance,PreviousClosingPrice&where=ValorId=",
                ISIN, currency,
                "4&columns=Date,Time,Close,Open,Low,High,TotalVolume&fromdate=19880630&netting=1440") # netting=1440 is equal to 1d (1440 minutes = 24 hours)
  
  json <- jsonlite::fromJSON(url)
  # json %>% str()
  
  out <- lapply(json$valors$data, FUN = unlist) %>% 
    as_tibble() %>% 
    mutate(Date = ymd(Date),
           Close = as.double(Close),
           Open = as.double(Open),
           Low = as.double(Low),
           High = as.double(High),
           TotalVolume = as.double(TotalVolume),
           ISIN = ISIN)
  
  return(out)
}

get_six_prices_cache <- function(ISIN, currency = "CHF",
                                 reload_if_older_than = "1 month",
                                 cache_dir = "data/cache_six_prices/") {
  
  if (paste0(ISIN, "_", currency, ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, ISIN, "_", currency, ".RDS"))
    
    if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  # get from SIX
  print(paste0("get prices from six (", ISIN, " ", currency, ")"))
  suppressWarnings(out <- get_six_prices(ISIN, currency))
  
  if (!is_tibble(out)) {
    return(NA)
    stop("no data available")
  }
  
  # out <- out %>% 
  #   mutate(ISIN = ISIN)
  
  # save to cached files
  list(loaddate = Sys.Date(),
       data = out) %>% 
    saveRDS(paste0(cache_dir, ISIN, "_", currency, ".RDS"))
  
  return(out)
}

get_cs_prices <- function(ISIN, currency) {
  url <- paste0("https://amfunds.credit-suisse.com/ch/de/institutional/fund/history/", ISIN, "?currency=", currency)
  
  download.file(url, destfile = "scrapedpage.xls", quiet=TRUE)
  
  data <- read_excel(url)
}



get_exchange_rate <- function(cur1, cur2, 
                              quandl_key = read_file("data/quandl.key")) {
  require(Quandl)
  
  Quandl.api_key(quandl_key)
  
  if (cur1 == "EUR") {
    out <- Quandl(paste0("ECB/", cur1, cur2), start_date="2000-01-01") %>% 
      as_tibble()
    return(out %>% rename(!!paste0(cur1, cur2) := 2))
  }
  if (cur2 == "EUR") {
    out <- Quandl(paste0("ECB/", cur2, cur1), start_date="2000-01-01") %>% 
      as_tibble()
    
    out <- out %>% 
      mutate(Value = 1/Value)
    
    return(out %>% rename(!!paste0(cur2, cur1) := 2))
  }
  
  EURcur1 <- Quandl(paste0("ECB/EUR", cur1))
  EURcur2 <- Quandl(paste0("ECB/EUR", cur2))
  
  currencies <- left_join(EURcur1 %>% rename(EURcur1 = 2), 
                          EURcur2 %>% rename(EURcur2 = 2),
                          by = "Date") %>% 
    as_tibble()
  
  out <- currencies %>% 
    mutate(!!paste0(cur1, cur2) := 1/EURcur1 * EURcur2) %>% 
    select(-EURcur1, -EURcur2)
  
  return(out)
}

get_exchange_rate_cache <- function(cur1, cur2, quandl_key = read_file("data/quandl.key"),
                                    reload_if_older_than = "1 week", cache_dir = "data/cache_exchangerates/") {
  
  if (paste0(cur1, cur2, ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, cur1, cur2, ".RDS"))
    
    if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  # get from Quandl
  print("get rates from Quandl")
  suppressWarnings(out <- get_exchange_rate(cur1, cur2, quandl_key = quandl_key))
  
  if (!is_tibble(out)) {
    out <- tibble(Date = Sys.Date(), !!paste0(cur1, cur2) := NA_real_) %>% 
      filter(F)
  }
  
  # save to cached files
  list(loaddate = Sys.Date(),
       data = out) %>% 
    saveRDS(paste0(cache_dir, cur1, cur2, ".RDS"))
  
  return(out)
}







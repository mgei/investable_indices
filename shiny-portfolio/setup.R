library(tidyverse)
library(tidyquant)
library(rvest)
library(lubridate)
library(scales)

library(shiny)
library(shinyWidgets)

get_yahoo_prices_cache <- function(symbol, 
                                   from = (floor_date(Sys.Date() - period(10, units = "years"), "month") - 1), 
                                   to = (floor_date(Sys.Date(), "month") - 1),
                                   cache_dir = "data/cache_yahoo_prices/") {
  if (!is.Date(from)) {
    from <- ymd(from)
  }
  if (!is.Date(to)) {
    to <- ymd(to)
  }
  if (is.na(from) | is.na(to)) {
    return(NA)
  }
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  
  out <- tibble()
  
  for (s in symbol) {
    if (paste0(s, ".RDS") %in% list.files(cache_dir)) {
      cached <- readRDS(paste0(cache_dir, s, ".RDS"))
      
      if (cached$from <= from & cached$to >= to) {
        o <- cached$data
        
        o <- o %>% 
          filter(date >= from,
                 date <= to)
        
        out <- out %>% 
          bind_rows(o)
        
        next()
      }
    }
    
    # get from yahoo finance
    print("get prices from yh")
    suppressWarnings(o <- tq_get(x = s, from = "1950-01-01", to = Sys.Date(), complete_cases = T, warnings = F))
    
    # get currency
    url <- paste0("https://finance.yahoo.com/quote/", s, "/history?p=", s)
    html <- read_html(url)
    
    text <- html %>% 
      # html_nodes("div .D\\(ib\\)") %>%
      html_nodes("span") %>%
      html_text()
    
    currency <- str_sub(text[grep("Currency in", text)], -3, -1)[1]
    
    if (length(currency) != 1) {
      currency <- ""
    }
    
    if (!is_tibble(o)) {
      o <- tibble(date = Sys.Date(),
                  open = NA_real_, high = NA_real_, low = NA_real_, close = NA_real_, volume = NA_real_, adjusted = NA_real_, 
                  currency = NA_character_) %>% 
        filter(F)
    }
    
    o <- o %>% 
      mutate("currency" = currency, 
             "symbol" = s) %>% 
      filter(date >= from,
             date <= to)
    
    # save to cached files
    list(from = from,
         to = to,
         data = o,
         loaddate = Sys.Date()) %>% 
      saveRDS(paste0(cache_dir, s, ".RDS"))
    
    
    out <- out %>% 
      bind_rows(o)
  }
  
  return(out)
}

get_IB_etflist_cache <- function(exchange = "ARCA",
                                 cache_dir = "data/cache_IB_etflist/",
                                 reload_if_older_than = "1 month") {
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  
  if (paste0(exchange, ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, exchange, ".RDS"))
    
    if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  
  if (exchange == "ARCA") {
    print("get from IB")
    html <- read_html("https://www.interactivebrokers.com/en/index.php?f=567&exch=arca")
    
    out <- html %>% 
      html_table() %>% 
      .[[3]] %>% 
      as_tibble() %>% 
      select(symbol = Symbol, description = 2, currency = Currency) %>% 
      arrange(symbol)
  } else {
    print("error")
    out <- tibble()
  }
  
  # save to cached files
  list(data = out,
       loaddate = Sys.Date()) %>% 
    saveRDS(paste0(cache_dir, exchange, ".RDS"))
  
  return(out)
}

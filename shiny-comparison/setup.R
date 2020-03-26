library(shiny)
library(tidyverse)
library(tidyquant)
library(lubridate)
# library(shinyWidgets)
# library(shinysky)
library(plotly)

stocks <- readRDS("data/cache/AAPL.RDS")$data


cache_dir <- "data/cache/"

get_prices <- function(symbol, 
                       from = (floor_date(Sys.Date() - period(10, units = "years"), "month") - 1), 
                       to = (floor_date(Sys.Date(), "month") - 1)) {
  if (!is.Date(from)) {
    # print("from isnt a date")
    from <- ymd(from)
  }
  if (!is.Date(to)) {
    # print("to isnt a date")
    to <- ymd(to)
  }
  if (is.na(from) | is.na(to)) {
    stop("dates to/from are invalid")
  }
                         
  if (paste0(symbol, ".RDS") %in% list.files(cache_dir)) {
    # print("exists in chache")
    cached <- readRDS(paste0(cache_dir, symbol, ".RDS"))
    
    if (cached$from <= from & cached$to >= to) {
      # print("only using cached data")
      out <- cached$data
      
      out <- out %>% 
        filter(date >= from,
               date <= to)
      
      return(out)
    }
  }
  
  # get from yahoo finance
  # print("get data from yh")
  out <- tq_get(x = symbol, from = from, to = to) %>% 
    mutate("symbol" = symbol)
  
  if (exists("cached")) {
    # print("cached was loaded exists")
    out <- anti_join(cached$data, out, by = "date") %>% 
      bind_rows(out)
    
    from <- min(from, min(out$date))
    to <- max(to, max(out$date))
  }
  
  # save to cached files
  list(from = from,
       to = to,
       data = out) %>% 
    saveRDS(paste0(cache_dir, symbol, ".RDS"))
  
  return(out)
}

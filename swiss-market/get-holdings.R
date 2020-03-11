# library(tidyverse)
library(rvest)

# SMICHA.SW
# SPY

get_holdings <- function(symbol) {
  url <- paste0("https://finance.yahoo.com/quote/", symbol, "/holdings")
  
  html <- read_html(url)
  cast <- html_nodes(html, ".Mb\\(20px\\)")
  table <- html_text(cast, trim = T)
  
  return(table)
}

get_holdings("SPY")

get_holdings("SMICHA.SW")
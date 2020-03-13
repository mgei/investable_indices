# library(tidyverse)
library(rvest)
library(magrittr)

# SMICHA.SW
# SPY

get_holdings <- function(symbol) {
  url <- paste0("https://finance.yahoo.com/quote/", symbol, "/holdings")
  
  html <- read_html(url)
  cast<-html %>%
    html_nodes("table tr") %>%
    html_nodes("td") %>%
    html_text()
  
  table <- data.frame(matrix(unlist(cast), ncol=3, byrow=T), stringsAsFactors = F) %>% 
    as_tibble() %>% 
    rename(Company = 1, Symbol = 2, holding = 3) %>% 
    mutate(holding_num = str_remove(holding, "%") %>% as.double() %>% divide_by(100))
  
  return(table)
}

get_holdings("BIM.SW")

get_holdings("AAPL")


##########

library(readxl)

etf_list <- read_excel("explorer_export_en.xls", skip = 4)

etf_list

# get etf data

## 1. get the latest complete list of ETFs from etfdb.com ----

# URL: https://etfdb.com/screener/#page=1

# We use the etfdb.com API https://github.com/janlukasschroeder/etfdb-api

# data.js:
# # const etfdb = require('etfdb-api');
# # 
# # // list all ETFs, sorted by year-to-date return, descending sort direction
# # etfdb
# # .listEtfs((perPage = 2321), (page = 1), (sort = 'assets'), (order = 'desc')) 
# # .then(result => {
# #   //console.log('Total ETFs:', result.meta.total_records);
# #   console.log('etf.symbol.text; etf.name.text; etf.mobile_title; etf.assets; etf.average_volume; etf.asset_class; etf.ytd; etf.inception');
# #   result.data.forEach(etf => console.log(etf.symbol.text, ';', etf.name.text, ';', etf.mobile_title, ';', etf.assets, ';', etf.average_volume, ';', etf.asset_class, ';', etf.ytd, ';', etf.date));
# # });

# Then  run it with:
# # node data.js > output.csv2

# This generates output.csv2

# read this!

library(tidyverse)
library(lubridate)

etfs <- read_csv2("output.csv2") %>%
  mutate_all(str_trim) %>% 
  mutate(etf.assets = etf.assets %>% str_remove_all("\\$|,") %>% as.double(),
         etf.average_volume = etf.average_volume %>% str_remove_all("\\$|,") %>% as.double(),
         etf.ytd = etf.ytd %>% str_remove_all("%") %>% as.double() %>% divide_by(100))

# mark LETFs, we'll not want those
etfs %<>% 
  mutate(leverage = case_when(str_detect(etf.name.text, "Short-Term|Short Term|Short Duration|Short Maturity|Short Treasury") ~ F,
                              str_detect(etf.name.text, "2x|3x|2X|3X|4x|4X|leverage|Leverage|inverse|Inverse|UltraPro|Ultra") ~ T,
                              str_detect(etf.name.text, "Short|short") ~ T,
                              T ~ F))

## 2. get price data from Yahoo Finance ----

library(tidyquant)

if (!file.exists("pricedata")) {
  dir.create("pricedata")
}

# we only want the non-leveraged ETFs. Here's the symbols that we want:
symbols <- etfs %>% 
  filter(!leverage,
         etf.assets > 100) %>% 
  pull(etf.symbol.text)

# get monhtly price data from Yahoo Finance (this takes a couple of minutes)
for (symbol in symbols) {
  print(str_c(which(symbols == symbol), " ", symbol))
  assign(symbol, tq_get(symbol, get  = "stock.prices", from = "2000-01-01", to = Sys.Date()))
  
  write_csv(get(symbol), str_c("pricedata/", symbol, ".csv"))
  
  rm(list=symbol)
  print("done")
}

directory <- list.files("pricedata/.")

# empty tibble for prices, needs the date column
pricedata <- tibble(date = as.Date("1900-01-01")) %>% 
  filter(F)

for (file in directory) {
  print(paste(which(directory == file), file))
  # make it monthly
  temp <- read_csv(paste0("pricedata/", file), col_types = "Ddddddd") %>% 
    tq_transmute(select     = open:adjusted, 
                 mutate_fun = to.period, 
                 period     = "months") %>% 
    mutate(date = ceiling_date(date, unit = "months") - days(1)) %>% 
    select(date, !!str_remove(file, ".csv") := adjusted)
  
  pricedata %<>% full_join(temp, by = "date")
  rm(temp)
  
  print("done")
}

pricedata

## 3. inception date issue ----

# So some tickers might have existed before the ETF that signs under that ticker name has been incepted.
# Yahoo Finance might have given us prices of some later-delisted pennystock.
# Therefore we shall cut off the data from previous to the official inception date.

# Let's get the inception dates from etfdb.com launch center

# URL: https://etfdb.com/etf-launch-center/

# # uncomment to run:
# inception_dates <- GET("https://etfdb.com/etf-launch-center/#Launches-2011")
#   
# inception_dates <- inception_dates %>% read_html() %>% html_table()
# 
# inception_dates_tibble <- lapply(inception_dates, as_tibble) %>% bind_rows() 
#   
# inceptions <- inception_dates_tibble %>%
#   filter(!is.na(Date)) %>% 
#   mutate(Date = mdy(Date)) %>% 
#   select(symbol = Ticker, inception = Date)
# 
# inceptions %>% write_csv("inceptions.csv")

inceptions <- read_csv("inceptions.csv")

pricedata <- pricedata %>% 
  arrange(date) %>% 
  gather(symbol, price, -date) %>% 
  filter(!is.na(price)) %>% 
  left_join(inceptions, by = "symbol") %>% 
  filter(date >= inception) %>% 
  select(-inception) %>% 
  distinct() %>% 
  spread(symbol, price)

## 4. compute returns ----

returns <- pricedata %>% 
  arrange(date) %>% 
  gather(symbol, price, -date) %>%
  filter(!is.na(price)) %>% 
  group_by(symbol) %>% 
  filter(min(date) <= periods_from[y]) %>% 
  tq_transmute(select     = price, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic") %>% 
  ungroup()

returns %>% 
  saveRDS("returns.RDS")

# returns %>% 
#   ggplot(aes(x = monthly.returns, y = symbol)) +
#   geom_point(size = 0.1) +
#   theme(axis.title.y = element_blank(), 
#         axis.ticks.y = element_blank(), 
#         axis.line.y = element_blank(), 
#         axis.text.y = element_blank())

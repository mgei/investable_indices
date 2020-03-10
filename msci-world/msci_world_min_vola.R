# MSCI World Min Vola
library(tidyverse)
library(readxl)
library(tidyquant)
library(lubridate)
library(Quandl)
quandl_api_key("")

library(scales)


index <- read_excel("historyIndex.xls", skip = 6) %>% 
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>% 
  select(Date, ACWI_min_vola = 2, world = 4)

index %>% 
  pivot_longer(names_to = "index", values_to = "value", -Date) %>% 
  filter(Date > as.Date("2000-01-01")) %>% 
  group_by(index) %>% 
  mutate(value = value/value[1]) %>% 
  ggplot(aes(x = Date, y = value, color = index)) +
  geom_line()

index %>% 
  pivot_longer(names_to = "index", values_to = "value", -Date) %>% 
  filter(Date > as.Date("2000-01-01")) %>% 
  group_by(index) %>%
  tq_transmute(select     = value, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra") %>% 
  tq_performance(Ra = Ra, performance_fun = table.AnnualizedReturns)


minvola_etfs <- read_excel("etf_list.xlsx") %>% 
  select(1:5)

etfs <- tibble()
for (i in 1:nrow(minvola_etfs)) {
  if (paste0(minvola_etfs[i, "isin"], ".xlsx") %in% list.files()) {
    temp <- read_excel(paste0(minvola_etfs[i, "isin"], ".xlsx")) %>% 
      transmute(Date = dmy(Datum),
                close = Schluss) %>% 
      mutate(name = minvola_etfs[i, "name"] %>% pull(),
             isin = minvola_etfs[i, "isin"] %>% pull(),
             curr = minvola_etfs[i, "curr"] %>% pull())
  }
  
  etfs <- bind_rows(etfs, temp)
}

etfs %>% 
  group_by(name, isin, curr) %>% 
  count()

eurchf <- Quandl("ECB/EURCHF", start_date="2013-01-01") %>% as_tibble() %>% select(Date, eurchf = Value)
eurusd <- Quandl("ECB/EURUSD", start_date="2013-01-01") %>% as_tibble() %>% select(Date, eurusd = Value)


etfs_chf <- etfs %>% 
  left_join(eurchf, by = "Date") %>% 
  left_join(eurusd, by = "Date") %>% 
  mutate(EUR = if_else(curr == "USD", (close/eurusd), close),
         CHF = EUR*eurchf)

etfs_chf %>% 
  group_by(name) %>% 
  summarise(min(Date))

etfs_chf %>% 
  filter(Date >= as.Date("2020-01-01")) %>% 
  arrange(Date) %>% 
  group_by(name) %>% 
  mutate(CHF1 = CHF/first(CHF)) %>%
  ggplot(aes(x = Date, y = CHF1, color = str_sub(name,1, 20))) +
  geom_line() +
  scale_y_continuous(labels = percent)

# # DOES NOT WORK
# library(rvest)
# 
# url <- "https://www.finanzen.ch/etf/historisch/ishares-edge-msci-world-minimum-volatility-etf-ie00b8fhgs14/swx/1.1.2006_10.3.2020"
# 
# html <- read_html(url)
# cast <- html_nodes(html, "#historic-price-list .box")
# html_text(cast, trim = TRUE)

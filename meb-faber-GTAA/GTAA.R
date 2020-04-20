library(tidyverse)
library(readxl)
library(tidyquant)
library(lubridate)

funds <- read_excel("GTAA-Top3.xlsx", skip = 2)

funds <- funds %>% 
  select(Asset, Details, ISIN, Currency) %>% 
  mutate(Asset_sector = if_else(is.na(ISIN), Asset, NA_character_)) %>% 
  mutate(Asset_sector = na.locf(Asset_sector)) %>% 
  filter(!is.na(ISIN))

source("GTAA-helper.R")

in_cache <- function(ISIN, currency, cache_dir = "cache/") {
  paste0(ISIN, "_", currency, ".RDS") %in% list.files(cache_dir)
}

# here's the funds that we can get data from SIX
funds %>% 
  mutate(data = in_cache(ISIN, Currency)) %>% 
  filter(data) %>% 
  print(n = 100)

library(httr)
library(rvest)

# url <- "https://amfunds.credit-suisse.com/ch/de/institutional/fund/detail/CH0191014973"
# 
# out <- read_html(url)
# 
# out %>% 
#   html_text()
# 
# 
# funds$ISIN[1]
# 
# data <- read_excel(paste0("CSIF/data_", funds$ISIN[1], ".xls"))
# read_html("CSIF/data_CH0017844686.xls") %>% 
#   html_table(fill=T) %>% 
#   .[[1]]
# 
# html <- read_html("CSIF/data_CH0017844686.xls")
# 
# data <- html %>% 
#   html_nodes("table") %>% 
#   html_table(header = T, fill = T) %>%
#   .[[1]] %>% 
#   as_tibble(.name_repair = "unique") %>% 
#   filter(row_number() > 4) %>%  # skip rows
#   magrittr::set_colnames(.[1,] %>% unlist(use.names = F)) %>% 
#   filter(row_number() > 1) %>%  # skip rows
#   mutate(`NAV Date` = dmy(`NAV Date`),
#          NAV = as.double(NAV),
#          ADDI = as.double(ADDI)) %>% 
#   filter(!is.na(NAV))

data <- tibble()
for (i in 1:nrow(funds)) {
  print(i)
  
  if (in_cache(funds$ISIN[i], funds$Currency[i])) {
    d <- get_six_prices_cache(funds$ISIN[i], funds$Currency[i], cache_dir = "cache/")
    
    out <- d %>% 
      mutate(Asset = funds$Asset[i], ISIN = funds$ISIN[i], Currency = funds$Currency[i]) %>% 
      select(Asset, ISIN, Currency, Date, Value = Close)
  } else {
    d <- read_html(paste0("CSIF/data_", funds$ISIN[i], ".xls")) %>% 
      html_nodes("table") %>% 
      html_table(header = T, fill = T) %>%
      .[[1]] %>% 
      as_tibble(.name_repair = "unique") %>% 
      filter(row_number() > 4) %>%  # skip rows
      magrittr::set_colnames(.[1,] %>% unlist(use.names = F)) %>% 
      filter(row_number() > 1) %>%  # skip rows
      mutate(`NAV Date` = dmy(`NAV Date`),
             NAV = as.double(NAV),
             ADDI = as.double(ADDI)) %>% 
      filter(!is.na(NAV))
    
    out <- d %>% 
      mutate(Asset = funds$Asset[i], ISIN = funds$ISIN[i]) %>% 
      select(Asset, ISIN, Currency = Measure, Date = `NAV Date`, Value = NAV)
  }
  
  data <- bind_rows(data, out)
}

data <- readRDS("data.RDS")

returns <- data %>% 
  group_by(Asset, ISIN, Currency) %>% 
  arrange(Date) %>% 
  tq_mutate(select = Value, mutate_fun = SMA, n = 200) %>% 
  group_by(Asset, ISIN, Currency, year_month = floor_date(Date, "months")) %>% 
  filter(Date == max(Date)) %>% 
  group_by(Asset, ISIN, Currency) %>% 
  mutate(Ra = Value/lag(Value)-1,
         rule = lag(Value)>lag(SMA),
         Ra_rule = Ra*rule) %>%
  filter(!is.na(rule))

currencies <- tibble()
for (cur in unique(data$Currency)) {
  temp <- get_exchange_rate_cache(cur, "CHF", quandl_key = read_file("quandl.key"), cache_dir = "cache/") %>% 
    rename(rate = 2) %>% 
    mutate(cur = cur)
    
  currencies <- bind_rows(currencies, temp)
}


dataCHF <- data %>% 
  arrange(Asset, ISIN, Currency, Date) %>% 
  left_join(currencies, by = c("Date", "Currency" = "cur")) %>% 
  mutate(rate = na.locf(rate)) %>% 
  mutate(ValueCHF = Value*rate)

dataCHF_R <- dataCHF %>% 
  group_by(Asset, ISIN) %>% 
  mutate(R_21d = ValueCHF/lag(ValueCHF, 21),
         R_63d = ValueCHF/lag(ValueCHF, 63),
         R_125d = ValueCHF/lag(ValueCHF, 125),
         R_250d = ValueCHF/lag(ValueCHF, 250),
         mean_21_63_125_125 = (R_21d + R_63d + R_125d + R_250d)/4) %>% 
  tq_mutate(select = ValueCHF, mutate_fun = SMA, n = 200)

monthly_top3 <- dataCHF_R %>% 
  group_by(Asset, ISIN, Currency, year_month = floor_date(Date, "months")) %>% 
  filter(Date == max(Date)) %>%
  ungroup() %>% 
  group_by(year_month = ceiling_date(Date, "months")) %>% 
  top_n(3, mean_21_63_125_125) %>% 
  ungroup() %>% 
  mutate(top3 = T) %>% 
  select(Asset, ISIN, Date, top3)

dataCHF_R %>%
  select(Asset, ISIN, Currency, Date, Value, rate, ValueCHF, SMA) %>% 
  left_join(monthly_top3, by = c("Asset", "ISIN", "Date")) %>% 
  mutate(ruleSMA = SMA > ValueCHF,
         weight = top3*ruleSMA*(1/3)) %>% 
  mutate(performance = lag(weight)*(ValueCHF/lag(ValueCHF)-1)) %>% 
  group_by(year_month = ceiling_date(Date, "months")) %>% 
  summarise(performance = prod(1+performance, na.rm = T)-1) %>% 
  mutate(p = cumprod(1+performance)-1) %>% 
  ggplot(aes(x = year_month, y = p)) +
  geom_line()


  mutate(Ra = adjusted/lag(adjusted)-1,
         rule = lag(adjusted)>lag(SMA),
         Ra_rule = Ra*rule) %>%
  filter(!is.na(rule)) 
  

returnsPAST <- returns %>% 
  mutate(Ra_21d = Value/lag(Value))

returns_years <- returns %>% 
  group_by(Asset, ISIN, Currency, year = year(Date)) %>% 
  summarise(Ra_year = (prod(1+Ra)-1),
            Ra_rule_year = (prod(1+Ra_rule)-1),
            Vola_year = sd(Ra)*sqrt(250),
            Vola_rule_year = sd(Ra_rule)*sqrt(250)) %>% 
  mutate(SR = Ra_year/Vola_year,
         SR_rule = Ra_rule_year/Vola_rule_year)

returns_years

returns_years %>% 
  select(-starts_with("Ra"), -starts_with("Vola")) %>% 
  mutate(rule_beats = SR_rule > SR) %>% 
  summarise(rule_beats = sum(rule_beats, na.rm = T)/n()) %>% 
  arrange(desc(rule_beats))

returns %>% 
  select(Asset, ISIN, Date, Currency, Ra, Ra_rule) %>% 
  pivot_longer(cols = -c("Asset", "ISIN", "Currency", "Date")) %>%
  arrange(Asset, ISIN, Currency, name, Date) %>% 
  group_by(Asset, ISIN, Currency, name) %>% 
  mutate(cum_ret = cumprod(1+value)-1) %>% 
  ggplot(aes(x = Date, y = cum_ret, color = name)) +
  scale_y_continuous(labels = scales::percent) +
  geom_line() +
  labs(x = "") +
  facet_wrap(~Asset, scales = "free_y") +
  theme(legend.position = c(0.8,0.05))
  


# SPY
# EFA
# IEF
# GSG
# FFR
etf_symbols <- c("SPY", "EFA", "IEF", "GSG", "FFR")

etf_prices <- tq_get(etf_symbols, from = "2000-01-01")

etf_prices

returns <- etf_prices %>% 
  group_by(symbol) %>% 
  arrange(date) %>% 
  tq_mutate(select = adjusted, mutate_fun = SMA, n = 200) %>% 
  group_by(symbol, year_month = floor_date(date, "months")) %>% 
  filter(date == max(date)) %>% 
  group_by(symbol) %>% 
  mutate(Ra = adjusted/lag(adjusted)-1,
         rule = lag(adjusted)>lag(SMA),
         Ra_rule = Ra*rule) %>%
  filter(!is.na(rule)) 

returns

returns_years <- returns %>% 
  group_by(symbol, year = year(date)) %>% 
  summarise(Ra_year = (prod(1+Ra)-1),
            Ra_rule_year = (prod(1+Ra_rule)-1),
            Vola_year = sd(Ra)*sqrt(250),
            Vola_rule_year = sd(Ra_rule)*sqrt(250)) %>% 
  mutate(SR = Ra_year/Vola_year,
         SR_rule = Ra_rule_year/Vola_rule_year)

returns_years

returns_years %>% 
  select(-starts_with("Ra"), -starts_with("Vola")) %>% 
  mutate(rule_beats = SR_rule > SR) %>% 
  summarise(rule_beats = sum(rule_beats, na.rm = T)/n()) %>% 
  arrange(desc(rule_beats))

returns %>% 
  select(symbol, date, Ra, Ra_rule) %>% 
  pivot_longer(cols = -c("symbol", "date")) %>%
  arrange(symbol, name, date) %>% 
  group_by(symbol, name) %>% 
  mutate(cum_ret = cumprod(1+value)-1) %>% 
  ggplot(aes(x = date, y = cum_ret, color = name)) +
  scale_y_continuous(labels = scales::percent) +
  geom_line() +
  labs(x = "") +
  facet_wrap(~symbol, scales = "free_y") +
  theme(legend.position = c(0.8,0.05))

library(tidyverse)
library(tidyquant)

source("../shiny-comparison/setup.R")

# fundlist <- reload_fundlist("fundlist.RDS")
# fundlist %>% saveRDS("fundlist.RDS")
fundlist <- readRDS("fundlist.RDS")

trading_since <- function(ISIN, currency, cache_dir = "../shiny-comparison/data/cache_six_prices/") {
  temp <- readRDS(paste0(cache_dir, ISIN, "_", currency, ".RDS"))
  return(min(temp$data$Date))
}

has_dividend <- function(ISIN, currency, cache_dir = "../shiny-comparison/data/cache_dividends/") {
  temp <- readRDS(paste0(cache_dir, ISIN, "_", currency, ".RDS"))
  return(nrow(temp$data) > 0)
}

get_six_prices_cache_multi <- function(ISIN, currency, 
                                       col_name = NULL, col_currency = NULL,
                                       reload_if_older_than = "1 week", 
                                       cache_dir = "../shiny-comparison/data/cache_six_prices/") {
  
  if (length(ISIN) != length(currency)) {
    stop("ISIN and currency vectors have to be of the same length")
  }
  
  out <- tibble()
  
  for (i in 1:length(ISIN)) {
    temp <- get_six_prices_cache(ISIN[i], currency[i], reload_if_older_than, cache_dir)
    
    if (!is.null(col_name)) {
      temp <- temp %>% 
        mutate(Name = col_name[i])
    }
    if (!is.null(col_currency)) {
      temp <- temp %>% 
        mutate(Currency = col_currency[i])
    }
    
    out <- bind_rows(out, temp)
  }
  
  return(out)
}

get_exchange_rate_cache_multi <- function(cur1, cur2, quandl_key = read_file("../shiny-comparison/data/quandl.key"),
                                          reload_if_older_than = "1 week", cache_dir = "../shiny-comparison/data/cache_exchangerates/") {
  if (length(cur1) != length(cur2)) {
    if (length(cur1) == 1) {
      cur1 = rep(cur1, length(cur2))
    } else if (length(cur2) == 1) {
      cur2 = rep(cur2, length(cur1))
    } else {
      stop("cur1 and cur2 must be of equal length or one at most 1")
    }
  }
  
  out <- tibble()
  
  for (i in 1:length(cur1)) {
    temp <- get_exchange_rate_cache(cur1[i], cur2[i], quandl_key = quandl_key, 
                                    reload_if_older_than = reload_if_older_than, cache_dir = cache_dir) %>% 
      rename(rate = 2) %>% 
      mutate(from = cur1[i], to = cur2[i])
    
    out <- bind_rows(out, temp)
  }
  
  return(out)
}

exchange_rates <- get_exchange_rate_cache_multi(cur1 = unique(fundlist$`Trading currency`), cur2 = "CHF")

fundlist_since <- fundlist %>%
  rowwise() %>% 
  mutate(since = trading_since(ISIN = ISIN, currency = `Trading currency`)) %>% 
  ungroup()

fundlist_since %>% saveRDS("data/fundlist_since.RDS")

fundlist_since %>% 
  filter(since < (Sys.Date() - years(4))) %>% 
  group_by(`Asset class`, `Investment region`) %>% 
  count() %>% 
  ungroup() %>% 
  pivot_wider(names_from = "Asset class", values_from = "n") %>% 
  mutate(sumVar = rowSums(.[2:11], na.rm = T)) %>% 
  arrange(desc(sumVar)) %>%
  select(-sumVar) %>% 
  print(n = 100)
  
# Commodities 
# Gold
gold_selection <- fundlist_since %>% 
  filter(since < (Sys.Date() - years(4)),
         `Asset class` == "Commodities",
         `Product type` == "Exchange Traded Funds",
         str_detect(Name, "Gold"),
         !str_detect(Name, "hedged|Hedged"),
         !(str_detect(Name, "Swisscanto") & `Trading currency` != "USD")) %>% 
  arrange(`Management fee`)

for (i in 1:nrow(gold_selection)) {
  print(get_six_dividends_cache(gold_selection$ISIN[i], gold_selection$`Trading currency`[i], cache_dir = "../shiny-comparison/data/cache_dividends/"))
}


p <- get_six_prices_cache_multi(gold_selection$ISIN, gold_selection$`Trading currency`, gold_selection$Name, gold_selection$`Trading currency`) %>% 
  # Die Anteile der oben erwähnten Anteilsklassen werden per 25. Oktober 2019 (nach Handelsschluss) im nachfolgenden Verhältnis aufgeteilt („Split“):
  # UBS ETF (CH) – Gold, (USD) A-dis: 1:3
  # https://www.swissfunddata.ch/sfdpub/docs/a08-8172_02-20191022-de.pdf
  mutate(Close = if_else(Name == "UBS ETF (CH) - Gold (USD) A-dis" & Date < dmy("28.10.2019"), Close/3, Close)) %>% 
  left_join(exchange_rates, by = c("Date", "Currency" = "from")) %>% 
  mutate(CloseCHF = Close*rate) %>% 
  filter(Date >= Sys.Date() - years(4)) %>% 
  group_by(Name) %>% 
  mutate(p = CloseCHF/CloseCHF[1L]) %>% 
  ggplot(aes(x = Date, y = p, color = Name)) +
  geom_line() +
  theme(legend.position = "none")

plotly::ggplotly(p)

gold <- Quandl("LBMA/GOLD")

gold_official <- gold %>% 
  as_tibble() %>%
  arrange(Date) %>% 
  select(Date, Gold = `GPB (PM)`) %>% 
  # filter(Date >= min(exchange_rates$Date)) %>% 
  filter(Date > as.Date("2003-01-01")) %>% 
  mutate(currency = "GBP") %>% 
  left_join(exchange_rates, by = c("Date", "currency" = "from")) %>% 
  mutate(rate = na.locf(rate)) %>% 
  mutate(GoldCHF = Gold * rate)


gold_capm <- get_six_prices_cache_multi(gold_selection$ISIN, gold_selection$`Trading currency`, gold_selection$Name, gold_selection$`Trading currency`) %>% 
  left_join(exchange_rates, by = c("Date", "Currency" = "from")) %>%
  group_by(Name) %>% 
  mutate(rate = na.locf(rate)) %>% 
  ungroup() %>% 
  mutate(CloseCHF = Close*rate) %>% 
  left_join(gold_official %>% select(Date, GoldCHF), by = "Date") %>% 
  group_by(ISIN, Name, Currency) %>% 
  mutate(GoldCHF = na.locf(GoldCHF)) %>% 
  mutate(Ra = log(CloseCHF/lag(CloseCHF)),
         Rb = log(GoldCHF/lag(GoldCHF))) %>% 
  group_by(ISIN, Name, Currency, year = year(Date)) %>% 
  tq_performance(Ra = Ra, Rb = Rb, table.CAPM)

gold_capm %>% 
  select(Name, Currency, year, Beta) %>% 
  pivot_wider(names_from = year, values_from = Beta) %>% 
  ungroup() %>% 
  summarise_all(mean, na.rm = T)

get_six_prices_cache_multi(gold_selection$ISIN, gold_selection$`Trading currency`, gold_selection$Name, gold_selection$`Trading currency`) %>% 
  left_join(exchange_rates, by = c("Date", "Currency" = "from")) %>%
  group_by(Name) %>% 
  mutate(rate = na.locf(rate)) %>% 
  ungroup() %>% 
  mutate(CloseCHF = Close*rate,
         VolumeCHF = TotalVolume*Close*rate) %>% 
  group_by(Name, ISIN, Currency, year = year(Date)) %>% 
  summarise(meanVolumeCHF = mean(VolumeCHF)) %>% 
  group_by(year) %>% 
  top_n(5, meanVolumeCHF) %>%
  arrange(desc(year), desc(meanVolumeCHF)) %>% 
  ggplot(aes(x = year, y = meanVolumeCHF, fill = Name)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = 2005:2020) +
  coord_flip()
  
gold_reg_data <- get_six_prices_cache_multi(gold_selection$ISIN, gold_selection$`Trading currency`, gold_selection$Name, gold_selection$`Trading currency`) %>% 
  left_join(exchange_rates, by = c("Date", "Currency" = "from")) %>%
  group_by(Name) %>% 
  mutate(rate = na.locf(rate)) %>% 
  ungroup() %>% 
  mutate(CloseCHF = Close*rate,
         VolumeCHF = TotalVolume*Close*rate) %>%
  mutate(NameCur = interaction(Name, Currency)) %>% 
  select(Date, NameCur, Currency, CloseCHF) %>% 
  pivot_wider(names_from = "NameCur", values_from = "CloseCHF") %>% 
  mutate_if(is.numeric, na.locf0) %>% 
  janitor::clean_names()

get_six_prices_cache_multi(gold_selection$ISIN, gold_selection$`Trading currency`, gold_selection$Name, gold_selection$`Trading currency`) %>% 
  # Die Anteile der oben erwähnten Anteilsklassen werden per 25. Oktober 2019 (nach Handelsschluss) im nachfolgenden Verhältnis aufgeteilt („Split“):
  # UBS ETF (CH) – Gold, (USD) A-dis: 1:3
  # https://www.swissfunddata.ch/sfdpub/docs/a08-8172_02-20191022-de.pdf
  mutate(Close = if_else(Name == "UBS ETF (CH) - Gold (USD) A-dis" & Date < dmy("28.10.2019"), Close/3, Close)) %>% 
  left_join(exchange_rates, by = c("Date", "Currency" = "from")) %>%
  group_by(Name) %>% 
  mutate(rate = na.locf(rate)) %>% 
  ungroup() %>% 
  mutate(CloseCHF = Close*rate,
         VolumeCHF = TotalVolume*Close*rate) %>%
  mutate(NameCur = interaction(Name, Currency)) %>% 
  group_by(Name, Currency) %>% 
  mutate(Ra = CloseCHF/lag(CloseCHF)-1) %>% 
  group_by(Name, Currency, year = year(Date)) %>%
  filter(n() > 240 | year == 2020) %>% 
  summarise(n = n(),
            r = prod(1 + Ra, na.rm = T) - 1 ,
            v = sd(Ra, na.rm = T)*sqrt(n())) %>% 
  mutate(sr = r/v) %>% 
  # filter(year == 2017) %>% 
  arrange(desc(r)) %>% 
  mutate(year = as.integer(year)) %>% 
  select(Name, Currency, year, r) %>% 
  pivot_wider(names_from = year, values_from = r) %>% 
  select(Name, Currency, colnames(.) %>% sort()) %>% 
  ungroup() %>% 
  mutate(sum14_19 = (`2014` + `2015` + `2016` + `2017` + `2018` + `2019`)/6) %>% 
  mutate_if(is.double, scales::percent_format(acccuracy = 0.01)) %>% 
  arrange(desc(sum14_19))

gold_official %>% 
  group_by(year= year(Date)) %>% 
  mutate(GoldCHF = na.locf0(GoldCHF)) %>% 
  summarise(r = percent(last(GoldCHF)/first(GoldCHF)-1), accuracy = 0.01) %>% 
  pivot_wider(names_from = year, values_from = r)

get_six_prices_cache_multi(gold_selection$ISIN, gold_selection$`Trading currency`, gold_selection$Name, gold_selection$`Trading currency`) %>% 
  left_join(exchange_rates, by = c("Date", "Currency" = "from")) %>%
  group_by(Name) %>%
  mutate(rate = na.locf(rate)) %>% 
  ungroup() %>% 
  mutate(CloseCHF = Close*rate) %>% 
  filter(Date >= as.Date("2016-01-01")) %>% 
  group_by(Name) %>% 
  mutate(R = log(CloseCHF/lag(CloseCHF))) %>% 
  group_by(Name, year = year(Date)) %>%
  summarise(mu = sum(R, na.rm = T),
            sigma = sd(R, na.rm = T)*sqrt(n())) %>% 
  select(Name, year, mu) %>% 
  pivot_wider(names_from = "year", values_from = "mu") %>% 
  mutate_if(is.numeric, scales::percent)


x <- get_six_prices_cache_multi(gold_selection$ISIN, gold_selection$`Trading currency`, gold_selection$Name, gold_selection$`Trading currency`) %>% 
  left_join(exchange_rates, by = c("Date", "Currency" = "from")) %>%
  group_by(Name) %>% 
  mutate(rate = na.locf(rate)) %>% 
  ungroup() %>% 
  mutate(CloseCHF = Close*rate) %>% 
  filter(Date %in% c(as.Date("2020-01-03"),
                     as.Date("2020-04-01"))) 

x %>% 
  select(Date, Name, ISIN, Currency, CloseCHF) %>% 
  pivot_wider(names_from = "Date", values_from = "CloseCHF") %>% 
  mutate(return = log(`2020-04-01`/`2020-01-03`)) %>% 
  arrange(Name) %>% 
  print(n = 100)

get_exchange_rate_cache_multi()

trading_since(fundlist$ISIN[1], fundlist$`Trading currency`[1])

  arrange(desc(Spread)) %>% 
  select(ISIN, Name, `Product type`, Spread)

### other Commodities
  
fundlist_since %>% 
  filter(since <= Sys.Date()- years(4),
         `Asset class` == "Commodities",
         !str_detect(Underlying, "gold|Gold"),
         `Product type` == "Exchange Traded Funds",
         !str_detect(Name, "hedged|Hedged"),
         !str_detect(Underlying, "hedged|Hedged"),
         !str_detect(Underlying, "platinum|palladium|silver"),
         ) %>% 
  datatable()


fundlist_since %>% 
  filter(since <= Sys.Date()- years(4),
         `Product type` == "Exchange Traded Funds",
         `Asset class` == "Equity Developed Markets",
         !str_detect(Name, "hedged|Hedged"),
         !str_detect(Underlying, "hedged|Hedged"),
         `Investment region` == "Global",
         str_detect(Name, "MSCI World")) %>% 
  datatable(class = 'compact cell-border') %>% 
  formatStyle(columns = 0:39, fontSize = "70%")

## Global dev equity

msciworld_selection <- fundlist_since %>% 
  filter(`Asset class` == "Equity Developed Markets",
         `Investment region` == "Global",
         `Product type` == "Exchange Traded Funds",
         str_detect(Underlying, "MSCI World"),
         !str_detect(Name, "Hedged|hedged"),
         since < (Sys.Date() - years(4)))

msciworld_selection %>%  
  datatable(class = 'compact cell-border') %>% 
  formatStyle(columns = 0:39, fontSize = "70%")

for (i in 1:nrow(msciworld_selection)) {
  print(get_six_dividends_cache(msciworld_selection$ISIN[i], msciworld_selection$`Trading currency`[i], cache_dir = "../shiny-comparison/data/cache_dividends/"))
}

p <- get_six_prices_cache_multi(msciworld_selection$ISIN, msciworld_selection$`Trading currency`, msciworld_selection$Name, msciworld_selection$`Trading currency`) %>% 
  # Die Anteile der oben erwähnten Anteilsklassen werden per 25. Oktober 2019 (nach Handelsschluss) im nachfolgenden Verhältnis aufgeteilt („Split“):
  # UBS ETF (CH) – Gold, (USD) A-dis: 1:3
  # https://www.swissfunddata.ch/sfdpub/docs/a08-8172_02-20191022-de.pdf
  mutate(Close = if_else(Name == "UBS ETF (CH) - Gold (USD) A-dis" & Date < dmy("28.10.2019"), Close/3, Close)) %>% 
  left_join(exchange_rates, by = c("Date", "Currency" = "from")) %>% 
  mutate(CloseCHF = Close*rate) %>% 
  filter(Date >= Sys.Date() - years(4)) %>% 
  group_by(Name) %>% 
  mutate(p = CloseCHF/CloseCHF[1L]) %>% 
  ggplot(aes(x = Date, y = p, color = Name)) +
  geom_line() +
  theme(legend.position = "none")

plotly::ggplotly(p)

fundlist_since_prices <- fundlist_since %>% 
  rowwise() %>% 
  mutate(prices = list(get_six_prices_cache(ISIN = ISIN, currency = `Trading currency`, reload_if_older_than = "1 month", cache_dir = "../shiny-comparison/data/cache_six_prices/")))

fundlist_since_prices %>% 
  ungroup() %>% 
  unnest()


fundlist_since_prices_dividends <- fundlist_since_prices %>% 
  rowwise() %>% 
  mutate(dividends = list(get_six_dividends_cache(ISIN = ISIN, currency = `Trading currency`, 
                                                  reload_if_older_than = "1 month", cache_dir = "../shiny-comparison/data/cache_dividends/"))) %>% 
  ungroup()

fundlist_since_prices_dividends %>% 
  mutate(`D.` = map_lgl(dividends, ~(nrow(.x) > 0))) %>% 
  select(Name, `D.`)


tibble(x = 1:3, y = list(tibble(i = 1:3), tibble(i = 1:4), tibble(i = 3:4, p = 2)))

create_list <- function(x) {
  return(tibble(yo = x:(x+3)))
}

tibble(y = 1:4) %>% 
  rowwise() %>% 
  mutate(yolo = list(create_list(y))) %>% 
  unnest()

fundlist_since_prices <- fundlist_since_prices %>% 
  ungroup()


fundlist_since_prices %>% 
  filter(`Asset class` == "Equity Developed Markets",
         `Investment region` == "Global",
         `Product type` == "Exchange Traded Funds",
         str_detect(Underlying, "MSCI World"),
         !str_detect(Name, "Hedged|hedged"),
         since < (Sys.Date() - years(4))) %>% 
  unnest(prices, names_repair = "universal") %>% 
  group_by(ISIN...6, `Trading.currency`) %>% 
  mutate(performance = Close/Close[1L] - 1) %>% 
  ggplot(aes(x = Date, y = performance, color = Name)) +
  geom_line() +
  theme(legend.position = "bottom")

fundlist_since_prices %>% 
  filter(`Asset class` == "Equity Developed Markets",
         `Investment region` == "Global",
         `Product type` == "Exchange Traded Funds",
         str_detect(Underlying, "MSCI World"),
         !str_detect(Name, "Hedged|hedged"),
         since < (Sys.Date() - years(4))) %>% 
  select(Name, ISIN, `Trading currency`, `Management fee`, since)


fundlist_since_prices_dividends %>% 
  mutate(x = map(prices, ~Close*4))


fundlist_since_prices_dividends %>% 
  mutate(x = map(prices, paste))

head(fundlist_since_prices_dividends) %>% 
  select(Name, `Trading currency`, prices) %>% 
  mutate(x = map(prices, function(x) x %>% 1))


x <- head(fundlist_since, n = 2) %>% 
  rowwise() %>% 
  mutate(prices = list(get_six_prices_cache(ISIN = ISIN, currency = `Trading currency`, 
                                            reload_if_older_than = "1 month", cache_dir = "../shiny-comparison/data/cache_six_prices/") %>% 
                         mutate(cur = `Trading currency`))) %>% 
  ungroup() %>% 
  mutate(prices = map(prices, ~left_join(.x, exchange_rates %>% filter(to == "CHF"), by = c("cur" = "from", "Date" = "Date")))) %>% 
  mutate(prices = map(prices, ~.x %>% mutate(rate = na.locf0(rate))))

x$prices[[1]]

x <- fundlist_since_prices_dividends %>% 
  mutate(prices = map2(prices, `Trading currency`, ~.x %>% mutate(cur = .y)))

x$prices[[4]]

fundlist_since_prices_dividends %>% 
  mut


exchange_rates


y <- tibble(qsec = head(as_tibble(mtcars))$qsec,
            y = 1:6)



head(as_tibble(mtcars)) %>% 
  group_by(cyl) %>% 
  nest() %>% 
  mutate(data, map(data, .x %>% mutate(qsec = )))
  mutate(data = map(data, ~left_join(.x, y, by = "qsec"))) %>% 
  unnest()
  
  
  
  
  

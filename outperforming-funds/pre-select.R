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

fundlist_1 %>% 
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
gold_selection <- fundlist_1 %>% 
  filter(since < (Sys.Date() - years(4)),
         `Asset class` == "Commodities",
         str_detect(Name, "Gold"),
         !str_detect(Name, "hedged|Hedged"),
         !(str_detect(Name, "Swisscanto") & `Trading currency` != "USD")) %>% 
  arrange(`Management fee`)

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



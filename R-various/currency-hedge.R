# currency hedge or not

library(tidyverse)
library(tidyquant)
library(rvest)
library(Quandl)

# USA: S&P 500
spx <- "^GSPC"

# Japan: NIKKEI 225
nikkei <- "^N225"

# Germany: DAX
dax <- "^GDAXI"

# Mexico: IPC Mexico
ipc <- "^MXX"

# Israel: TA-125
ta <- "^TA125.TA"

# Indonesia: Jakarta Composite
jakarta <- "^JKSE"

# France: CAC 40
cac <- "^FCHI"

# UK: FTSE
ftse <- "^FTSE"

# Russia: MOEX
moex <- "IMOEX.ME"

# Hong Kong: HSI
hsi <- "^HSI"

# Singapour Index STI
sti <- "^STI"

# China: Shanghai Stock Exchange
sse <- "000001.SS"

# Australia: ASX
asx <- "^AXJO"

# India: Bombai Stock Exchange
bse <- "^BSESN"

# Malaysia: FTSE Bursa Malaysia
malaysia <- "^KLSE"

# New Zealand
nz <- "^NZ50"

# Korea
kospi <- "^KS11"

# Taiwan: TSEC
tsec <- "^TWII"

# Brazil
bvsp <- "^BVSP"

# Chile 
ipsa <- "^IPSA"

# Argentina
merval <- "^MERV"

# Egypt
egx <- "^CASE30"

# indices <- ls()
# dput(indices)
indices <- c("asx", "bse", "bvsp", "cac", "dax", "egx", "ftse", "hsi", "ipc", 
             "ipsa", "jakarta", "kospi", "malaysia", "merval", "moex", "nikkei", 
             "nz", "spx", "sse", "sti", "ta", "tsec")
vget <- Vectorize(get)

indices <- tibble(index = indices) %>% 
  mutate(symbol = vget(index))


get_yahoo <- function(symbol) {
  prices <- tq_get(symbol)
  
  url <- paste0("https://finance.yahoo.com/quote/", symbol, "/history?p=", symbol)
  html <- read_html(url)
  
  text <- html %>% 
    # html_nodes("div .D\\(ib\\)") %>%
    html_nodes("span") %>%
    html_text()
  
  currency <- str_sub(text[grep("Currency in", text)], -3, -1)[1]
  
  if (length(currency) == 0) {
    currency <- ""
  }
  
  prices <- prices %>% 
    mutate(currency = currency)
  
  return(prices)
} 

get_exchange_rate <- function(cur1, cur2, 
                              quandl_key = read_file("../quandl.key")) {
  Quandl.api_key(quandl_key)
  
  nodata <- tibble(Date = Sys.Date(), !!paste0(cur1, cur2) := 1) %>% filter(F)
  
  if (cur1 == "EUR") {
    out <- tryCatch({ 
      Quandl(paste0("ECB/", cur1, cur2), start_date="2000-01-01") %>% 
        as_tibble() %>% 
        rename(!!paste0(cur1, cur2) := 2)
    }, error = function(e) { nodata } )
    
    return(out)
  }
  if (cur2 == "EUR") {
    out <- tryCatch({
      Quandl(paste0("ECB/", cur2, cur1), start_date="2000-01-01") %>% 
        as_tibble() %>% 
        mutate(Value = 1/Value) %>% 
        rename(!!paste0(cur2, cur1) := 2)
    }, error = function(e) { nodata } )
    
    return(out)
  }
  
  EURcur1 <- tryCatch({ Quandl(paste0("ECB/EUR", cur1)) }, error = function(x) { tibble() } )
  EURcur2 <- tryCatch({ Quandl(paste0("ECB/EUR", cur2)) }, error = function(e) { tibble() } )
  
  if (nrow(EURcur1) !=0 & nrow(EURcur2) != 0) {
    currencies <- left_join(EURcur1 %>% rename(EURcur1 = 2), 
                            EURcur2 %>% rename(EURcur2 = 2),
                            by = "Date") %>% 
      as_tibble()
    
    out <- currencies %>% 
      mutate(!!paste0(cur1, cur2) := 1/EURcur1 * EURcur2) %>% 
      select(-EURcur1, -EURcur2)
  } else {
    out <- nodata
  }
  
  return(out)
}


out <- indices %>% 
  mutate(prices = map(symbol, ~get_yahoo(.x)))

out_cur <- out %>% 
  mutate(currency = map_chr(prices, ~.x[[1, "currency"]]))



currencies <- out_cur %>% 
  select(currency) %>% 
  mutate(rates = map(currency, ~get_exchange_rate(.x, "CHF")))

out_cur[[1,"prices"]]

currencies %>%
  mutate(rates = map(rates, ~.x %>% rename(CHF = 2))) %>% 
  unnest("rates")



data <- out_cur %>% 
  select(-symbol, -currency) %>% 
  unnest("prices") %>% 
  left_join(currencies %>%
              mutate(rates = map(rates, ~.x %>% rename(CHF = 2))) %>% 
              unnest("rates"),
            by = c("date" = "Date", "currency")) %>% 
  filter(!is.na(CHF)) %>% 
  arrange(index, date) %>% 
  mutate(adjustedCHF = adjusted/CHF) %>% 
  group_by(index, symbol) %>% 
  mutate(r = adjusted/lag(adjusted) - 1,
         rCHF = adjustedCHF/lag(adjustedCHF) - 1) %>% 
  filter(!is.na(r)) %>% 
  mutate(p = cumprod(1 + r) - 1,
         pCHF = cumprod(1 + rCHF) - 1)

data %>% 
  summarise(min(date))


data  %>% 
  filter(!index %in% c("ta", "moex", "merval")) %>%
  # filter(index == "merval") %>% 
  ggplot(aes(x = date, y = p, color = index)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom")

data  %>% 
  filter(!index %in% c("ta", "moex", "merval")) %>%
  # filter(index == "merval") %>% 
  ggplot(aes(x = date, y = pCHF, color = index)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom")


data  %>% 
  filter(!index %in% c("ta", "moex", "merval")) %>% 
  summarise(vola = sd(r)*sqrt(252),
            volaCHF = sd(rCHF)*sqrt(252)) %>% 
  mutate(difference = volaCHF - vola)

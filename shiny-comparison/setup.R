# options(shiny.reactlog = TRUE)

## packages ----
library(shiny)
# library(argonR)
# library(argonDash)
# library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(tidyquant)
library(lubridate)
library(shinyWidgets)
# library(shinysky)
# library(plotly)
library(httr)
library(readxl)
library(rvest)
library(DT)
library(magrittr)
library(scales)
library(Quandl)
library(RcppRoll)

widget_size <- "normal"  #"normal" # sm" # "xs"
fontsize <- "100%" # 80%

## functions ----

get_prices_cache <- function(symbol, 
                             from = (floor_date(Sys.Date() - period(10, units = "years"), "month") - 1), 
                             to = (floor_date(Sys.Date(), "month") - 1),
                             reload_if_older_than = "1 week",
                             cache_dir = "data/cache_prices/") {
  if (!is.Date(from)) {
    from <- ymd(from)
  }
  if (!is.Date(to)) {
    to <- ymd(to)
  }
  if (is.na(from) | is.na(to)) {
    return(NA)
  }
                         
  if (paste0(symbol, ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, symbol, ".RDS"))
    
    if (cached$from <= from & cached$to >= to) {
      out <- cached$data
      
      out <- out %>% 
        filter(date >= from,
               date <= to)
      
      return(out)
    } else if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  # get from yahoo finance
  print("get prices from yh")
  suppressWarnings(out <- tq_get(x = symbol, from = from, to = to, complete_cases = T, warnings = F))
  
  # get currency
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
  
  if (!is_tibble(out)) {
    out <- tibble(date = Sys.Date(),
                  open = NA_real_, high = NA_real_, low = NA_real_, close = NA_real_, volume = NA_real_, adjusted = NA_real_, 
                  currency = NA_character_) %>% 
      filter(F)
  }
  
  out <- out %>% 
    mutate("currency" = currency, 
           "symbol" = symbol)
  
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
       data = out,
       loaddate = Sys.Date()) %>% 
    saveRDS(paste0(cache_dir, symbol, ".RDS"))
  
  return(out)
}

get_six_details <- function(ISIN, currency = "CHF", category = "funds", output = "df") {
  
  if (category == "funds") {
    category_short <- "FU"
  } else if (category == "shares") {
    category_short <- "EQ"
  } else {
    stop("category has to be funds or shares")
  }
  
  url<-paste0("https://www.six-group.com/exchanges/", category, "/info_details_en.html?id=", ISIN, currency, "4&portalSegment=", category_short)
  
  D1 <- read_html(url) %>% 
    html_nodes("tr") %>%
    html_nodes("td.last") %>%
    html_text()
  
  Valor_symbol<-D1[1]
  Valor_number<-D1[2]
  ISIN<-D1[3]
  Trading_currency<-D1[4]
  Exchange<-D1[6]
  Product_type<-D1[7]
  Trading<-D1[8]
  Fund_type<-D1[9]
  Smallest_tradeable_unit<-D1[10]
  Asset_class<-D1[12]
  Domicile_of_fund<-D1[13]
  Investment_region<-D1[14]
  Management_style<-D1[16]
  Market_expectation<-D1[17]
  Replication_method<-D1[18]
  Fund_manager<-D1[20]
  Dividend_entitlement<-D1[22] 
  
  D2 <- read_html(url) %>% 
    html_nodes("tr") %>%
    #html_nodes("td.last") %>%
    html_text()
  
  Underlying<-D2[grep('Underlying',D2)]
  Underlying<-Underlying[2]
  Underlying<-gsub('Underlying','',Underlying)
  
  Index_provider<-D2[grep('Index provider',D2)]
  Index_provider<-Index_provider[2]
  Index_provider<-gsub('Index provider','',Index_provider)
  
  Number_in_issue<-D2[grep('Number in issue',D2)]
  Number_in_issue<-Number_in_issue[2]
  Number_in_issue<-gsub('Number in issue','',Number_in_issue)
  
  Fund_currency<-D2[grep('Fund currency',D2)]
  Fund_currency<-Fund_currency[2]
  Fund_currency<-gsub('Fund currency','',Fund_currency)
  
  Management_fee<-D2[grep('Management fee',D2)]
  Management_fee<-Management_fee[2]
  Management_fee<-gsub('Management fee','',Management_fee)
  
  Bloomberg_symbol<-D2[grep('Bloomberg symbol',D2)]
  Bloomberg_symbol<-Bloomberg_symbol[2]
  Bloomberg_symbol<-gsub('Bloomberg symbol','',Bloomberg_symbol)
  
  Reuters_symbol<-D2[grep('Reuters symbol',D2)]
  Reuters_symbol<-Reuters_symbol[2]
  Reuters_symbol<-gsub('Reuters symbol','',Reuters_symbol)
  
  if (output == "df") {
    out<-data.frame(Valor_symbol,Valor_number,ISIN,Trading_currency,Exchange,Product_type,Trading,Fund_type,Smallest_tradeable_unit,
                    Asset_class,Domicile_of_fund,Investment_region,Management_style,Market_expectation,Replication_method,Fund_manager,
                    Dividend_entitlement,Underlying,Index_provider,Number_in_issue,Fund_currency,Management_fee,Bloomberg_symbol,
                    Reuters_symbol, 
                    stringsAsFactors = F)
  } else if (output == "list"){
    out<-list(Valor_symbol=Valor_symbol,Valor_number=Valor_number,ISIN=ISIN,Trading_currency=Trading_currency,Exchange=Exchange,
              Product_type=Product_type,Trading=Trading,Fund_type=Fund_type,Smallest_tradeable_unit=Smallest_tradeable_unit,
              Asset_class=Asset_class,Domicile_of_fund=Domicile_of_fund,Investment_region=Investment_region,Management_style=Management_style,
              Market_expectation=Market_expectation,Replication_method=Replication_method,Fund_manager=Fund_manager,
              Dividend_entitlement=Dividend_entitlement,Underlying=Underlying,Index_provider=Index_provider,Number_in_issue=Number_in_issue,
              Fund_currency=Fund_currency,Management_fee=Management_fee,Bloomberg_symbol=Bloomberg_symbol,Reuters_symbol=Reuters_symbol)
  } else {
    stop("output has to be df or list")
  }
  
  return(out)
}

get_six_details_cache <- function(ISIN, currency = "CHF", category = "funds", output = "df",
                                  reload_if_older_than = "1 month",
                                  cache_dir = "data/cache_details/") {
  
  if (paste0(ISIN, "_", currency, ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, ISIN, "_", currency, ".RDS"))
    
    if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  # get from SIX
  print("get details from six")
  suppressWarnings(out <- get_six_details(ISIN, currency, category))
  
  # save to cached files
  list(loaddate = Sys.Date(),
       data = out) %>% 
    saveRDS(paste0(cache_dir, ISIN, "_", currency, ".RDS"))
  
  return(out)
}

get_six_dividends <- function(ISIN, currency = "CHF", category = "funds") {
  
  if (category == "funds") {
    category_short <- "FU"
  } else if (category == "shares") {
    category_short <- "EQ"
  } else {
    stop("category has to be funds or shares")
  }
  
  url<-paste0("https://www.six-group.com/exchanges/", category, "/info_details_en.html?id=", ISIN, currency,"4&portalSegment=", category_short)
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  
  D1 <- read_html("scrapedpage.html") %>% 
    html_nodes("table.table-grid") %>%
    html_nodes("td") %>%
    html_text()
  
  unlink("scrapedpage.html")
  df <- data.frame(matrix(unlist(D1), ncol=3, byrow=T), stringsAsFactors = F)
  colnames(df) <-c("Ex_dividend_date","Value","Currency")
  df <- df[-c(1, 2),]
  
  df <- df %>% 
    as_tibble() %>% 
    mutate(Ex_dividend_date = dmy(Ex_dividend_date),
           Value = as.double(Value))
  return(df)
}

get_six_dividends_cache <- function(ISIN, currency = "CHF", category = "funds", 
                                    reload_if_older_than = "1 month",
                                    cache_dir = "data/cache_dividends/") {
  
  if (paste0(ISIN, "_", currency, ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, ISIN, "_", currency, ".RDS"))
    
    if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  # get from SIX
  print("get dividends from six")
  suppressWarnings(out <- get_six_dividends(ISIN, currency, category))
  
  if (!is_tibble(out)) {
    return(NA)
    stop("no data available")
  }
  
  out <- out %>% 
    mutate(ISIN = ISIN)
  
  # save to cached files
  list(loaddate = Sys.Date(),
       data = out) %>% 
    saveRDS(paste0(cache_dir, ISIN, "_", currency, ".RDS"))
  
  return(out)
}

get_six_prices <- function(ISIN, currency = "CHF") {
  
  url <- paste0("https://www.six-group.com/itf/fqs/delayed/charts.json?select=ISIN,ClosingPrice,ClosingPerformance,PreviousClosingPrice&where=ValorId=",
                ISIN, currency,
                "4&columns=Date,Time,Close,Open,Low,High,TotalVolume&fromdate=19880630&netting=1440") # netting=1440 is equal to 1d (1440 minutes = 24 hours)
  
  json <- jsonlite::fromJSON(url)
  # json %>% str()
  
  out <- lapply(json$valors$data, FUN = unlist) %>% 
    as_tibble() %>% 
    mutate(Date = ymd(Date),
           Close = as.double(Close),
           Open = as.double(Open),
           Low = as.double(Low),
           High = as.double(High),
           TotalVolume = as.double(TotalVolume),
           ISIN = ISIN)
  
  return(out)
}

get_six_prices_cache <- function(ISIN, currency = "CHF",
                                 reload_if_older_than = "1 month",
                                 cache_dir = "data/cache_six_prices/") {
  
  if (paste0(ISIN, "_", currency, ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, ISIN, "_", currency, ".RDS"))
    
    if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  # get from SIX
  print(paste0("get prices from six (", ISIN, " ", currency, ")"))
  suppressWarnings(out <- get_six_prices(ISIN, currency))
  
  if (!is_tibble(out)) {
    return(NA)
    stop("no data available")
  }

  # out <- out %>% 
  #   mutate(ISIN = ISIN)
  
  # save to cached files
  list(loaddate = Sys.Date(),
       data = out) %>% 
    saveRDS(paste0(cache_dir, ISIN, "_", currency, ".RDS"))
  
  return(out)
}

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

get_holdings_cache <- function(symbol, 
                               reload_if_older_than = "1 month", cache_dir = "data/cache_holdings/") {
  
  if (paste0(symbol, ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, symbol, ".RDS"))
    
    if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  # get from SIX
  print("get holdings from yh")
  suppressWarnings(out <- get_holdings(symbol))
  
  if (!is_tibble(out)) {
    out <- tibble(Company = NA_character_, Symbol = NA_character_, holding = NA_character_, holding_num = NA_real_) %>% 
      filter(F)
  }
  
  # save to cached files
  list(loaddate = Sys.Date(),
       data = out) %>% 
    saveRDS(paste0(cache_dir, symbol, ".RDS"))
  
  return(out)
}

get_net_assets <- function(symbol) {
  url <- paste0("https://finance.yahoo.com/quote/", symbol, "/profile?p=", symbol)
  
  html <- read_html(url)
  
  out <- html %>% 
    html_nodes(".Pt\\(10px\\)") %>% 
    html_text() %>% 
    .[which(str_detect(., "^Net Assets"))] %>% 
    str_remove("Net Assets")
  
  if (is_empty(out)) {
    out <- ""
  }
  out <- out %>% str_c(collapse = " ")
  
  out
}

get_exchange_rate <- function(cur1, cur2, 
                              quandl_key = read_file("data/quandl.key")) {
  Quandl.api_key(quandl_key)
  
  if (cur1 == "EUR") {
    out <- Quandl(paste0("ECB/", cur1, cur2), start_date="2000-01-01") %>% 
      as_tibble()
    return(out %>% rename(!!paste0(cur1, cur2) := 2))
  }
  if (cur2 == "EUR") {
    out <- Quandl(paste0("ECB/", cur2, cur1), start_date="2000-01-01") %>% 
      as_tibble()
    
    out <- out %>% 
      mutate(Value = 1/Value)
    
    return(out %>% rename(!!paste0(cur2, cur1) := 2))
  }
  
  EURcur1 <- Quandl(paste0("ECB/EUR", cur1))
  EURcur2 <- Quandl(paste0("ECB/EUR", cur2))
  
  currencies <- left_join(EURcur1 %>% rename(EURcur1 = 2), 
                          EURcur2 %>% rename(EURcur2 = 2),
                          by = "Date") %>% 
    as_tibble()
  
  out <- currencies %>% 
    mutate(!!paste0(cur1, cur2) := 1/EURcur1 * EURcur2) %>% 
    select(-EURcur1, -EURcur2)
  
  return(out)
}
 
get_exchange_rate_cache <- function(cur1, cur2, quandl_key = read_file("data/quandl.key"),
                                    reload_if_older_than = "1 week", cache_dir = "data/cache_exchangerates/") {
  
  if (paste0(cur1, cur2, ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, cur1, cur2, ".RDS"))
    
    if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  # get from Quandl
  print("get rates from Quandl")
  suppressWarnings(out <- get_exchange_rate(cur1, cur2, quandl_key = quandl_key))
  
  if (!is_tibble(out)) {
    out <- tibble(Date = Sys.Date(), !!paste0(cur1, cur2) := NA_real_) %>% 
      filter(F)
  }
  
  # save to cached files
  list(loaddate = Sys.Date(),
       data = out) %>% 
    saveRDS(paste0(cache_dir, cur1, cur2, ".RDS"))
  
  return(out)
}

reload_fundlist <- function(path = "data/fundlist.RDS") {
  url <- "https://www.six-group.com/exchanges/funds/explorer_export_en.xls"
  GET(url, write_disk("data/fundlist.xls", overwrite = T))
  fundlist <- read_xls("data/fundlist.xls", skip = 4)
  
  fundlist <- fundlist %>% 
    mutate(`Management fee` = as.double(`Management fee`),
           `Mgmt fee` = `Management fee`/100)
  
  fundlist %>% saveRDS("data/fundlist.RDS")
  
  return(fundlist)
}

reload_six_benchmark <- function() {
  # SMI
  url <- "https://www.six-group.com/exchanges/downloads/indexdata/hsmi.csv"
  data <- read_csv2(url, skip = 1)
  
  smi <- data[-1:-4,] %>% 
    rename(Date = "SYMBOL") %>% 
    mutate(Date = as.Date(Date, "%d.%m.%Y")) %>% 
    mutate_if(is.character, funs(as.numeric(.))) %>% 
    select(date = Date, SMI, SMIC) %>% 
    arrange(date) %>% 
    mutate(SMI_return = SMI/lag(SMI)-1,
           SMIC_predividend = lag(SMIC)*(1+SMI_return),
           SMIC_dividend = SMIC - SMIC_predividend)
  
  # SPI
  url <- "https://www.six-group.com/exchanges/downloads/indexdata/hspitr.csv"
  data <- read_csv2(url, skip = 1) 
  
  spi_tr <- data[-1:-4,] %>% 
    rename(Date = "SYMBOL") %>% 
    mutate(Date = as.Date(Date, "%d.%m.%Y")) %>% 
    mutate_if(is.character, funs(as.numeric(.))) %>% 
    select(date = Date, SXGE) # SXGE is the TR index
  
  url <- "https://www.six-group.com/exchanges/downloads/indexdata/hspipr.csv"
  data <- read_csv2(url, skip = 1) 
  
  spi_pr <- data[-1:-4,] %>% 
    rename(Date = "SYMBOL") %>% 
    mutate(Date = as.Date(Date, "%d.%m.%Y")) %>% 
    mutate_if(is.character, funs(as.numeric(.))) %>% 
    select(date = Date, SPIX) # SPIX is the Price index
  
  
  spi <- spi_tr %>% 
    full_join(spi_pr, by = "date") %>% 
    filter(!is.na(SXGE),
           !is.na(SPIX)) %>% 
    arrange(date) %>% 
    mutate(SPIX_return = SPIX/lag(SPIX)-1,
           SXGE_predividend = lag(SXGE)*(1+SPIX_return),
           SXGE_dividend = SXGE - SXGE_predividend)
  
  smi_long <- smi %>% 
    pivot_longer(names_to = "series", values_to = "value", -date)
  
  spi_long <- spi %>% 
    pivot_longer(names_to = "series", values_to = "value", -date)
  
  benchmarks <- bind_rows(smi_long, spi_long)
  
  benchmarks %>% saveRDS("data/benchmarks.RDS")
  
  return(benchmarks)
}

get_six_index <- function(index = "SMI") {
  if (index == "SMI" | index == "SMIC") {
    url <- "https://www.six-group.com/exchanges/downloads/indexdata/hsmi.csv"
  } else if (index == "SXGE") {
    url <- "https://www.six-group.com/exchanges/downloads/indexdata/hspitr.csv"
  } else if (index == "SPIX") {
    url <- "https://www.six-group.com/exchanges/downloads/indexdata/hspipr.csv"
  } else {
    return(tibble())
  }
  
  suppressWarnings(suppressMessages(data <- read_delim(url, skip = 1, delim = ";")))
  
  out <- data[-1:-4, ] %>% 
    dplyr::rename(Date = SYMBOL) %>% 
    mutate(Date = as.Date(Date, "%d.%m.%Y")) %>% 
    mutate_if(is.character, as.numeric) %>% 
    select(Date, all_of(index)) %>% 
    arrange(Date)
  
  return(out)
}

get_six_index_cache <- function(index = "SMI",
                                reload_if_older_than = "1 month",
                                cache_dir = "data/cache_index/") {
  
  if (paste0(index, ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, index, ".RDS"))
    
    if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  # get from SIX
  print("get index from six")
  suppressWarnings(out <- get_six_index(index))
  
  if (!is_tibble(out)) {
    return(NA)
    stop("no data available")
  }
  
  # save to cached files
  list(loaddate = Sys.Date(),
       data = out) %>% 
    saveRDS(paste0(cache_dir, index, ".RDS"))
  
  return(out)
  
}

reload_quandle_exchangerates <- function(quandl_key = read_file("data/quandl.key")) {
  Quandl.api_key(quandl_key)
  
  EURCHF <- Quandl("ECB/EURCHF", start_date="2000-01-01") %>% as_tibble()
  EURAUD <- Quandl("ECB/EURAUD", start_date="2000-01-01") %>% as_tibble()
  EURCAD <- Quandl("ECB/EURCAD", start_date="2000-01-01") %>% as_tibble()
  EURGBP <- Quandl("ECB/EURGBP", start_date="2000-01-01") %>% as_tibble()
  EURHKD <- Quandl("ECB/EURHKD", start_date="2000-01-01") %>% as_tibble()
  EURJPY <- Quandl("ECB/EURJPY", start_date="2000-01-01") %>% as_tibble()
  EURNOK <- Quandl("ECB/EURNOK", start_date="2000-01-01") %>% as_tibble()
  EURSEK <- Quandl("ECB/EURSEK", start_date="2000-01-01") %>% as_tibble()
  EURSGD <- Quandl("ECB/EURSGD", start_date="2000-01-01") %>% as_tibble()
  EURUSD <- Quandl("ECB/EURUSD", start_date="2000-01-01") %>% as_tibble()
  
  currencies <- EURCHF %>% select(Date, EURCHF = Value) %>%
    full_join(EURAUD %>% select(Date, EURAUD = Value),
              by = "Date") %>% 
    full_join(EURCAD %>% select(Date, EURCAD = Value),
              by = "Date") %>%
    full_join(EURGBP %>% select(Date, EURGBP = Value),
              by = "Date") %>%
    full_join(EURHKD %>% select(Date, EURHKD = Value),
              by = "Date") %>%
    full_join(EURJPY %>% select(Date, EURJPY = Value),
              by = "Date") %>%
    full_join(EURNOK %>% select(Date, EURNOK = Value),
              by = "Date") %>%
    full_join(EURSEK %>% select(Date, EURSEK = Value),
              by = "Date") %>%
    full_join(EURSGD %>% select(Date, EURSGD = Value),
              by = "Date") %>%
    full_join(EURUSD %>% select(Date, EURUSD = Value),
              by = "Date")
  
  currencies <- currencies %>% 
    arrange(Date) %>% 
    mutate(AUDCHF = (1/EURAUD)*EURCHF,
           CADCHF = (1/EURCAD)*EURCHF,
           GBPCHF = (1/EURGBP)*EURCHF,
           HKDCHF = (1/EURHKD)*EURCHF,
           JPYCHF = (1/EURJPY)*EURCHF,
           NOKCHF = (1/EURNOK)*EURCHF,
           SEKCHF = (1/EURSEK)*EURCHF,
           SGDCHF = (1/EURSGD)*EURCHF,
           USDCHF = (1/EURUSD)*EURCHF) %>% 
    pivot_longer(-Date)
    
  currencies %>% 
    saveRDS("data/currencies.RDS")
  
}


## plotting helper ----

onRenderRebaseTxt <- "
    function(el, x) {
      el.on('plotly_relayout', function(rlyt) {
        var nrTrcs = el.data.length;
        // array of x index to rebase to; defaults to zero when all x are shown, needs to be one per trace
        baseX = Array.from({length: nrTrcs}, (v, i) => 0);
        // if x zoomed, increase baseX until first x point larger than x-range start
        if (el.layout.xaxis.autorange == false) {
            for (var trc = 0; trc < nrTrcs; trc++) {
                while (el.data[[trc]].x[baseX[trc]] < el.layout.xaxis.range[0]) {baseX[trc]++;}
            }   
        }
        // rebase each trace
        for (var trc = 0; trc < nrTrcs; trc++) {
            el.data[trc].y = el.data[[trc]].y.map(x => x / el.data[[trc]].y[baseX[trc]]);
        }
        el.layout.yaxis.autorange = true; // to show all traces if y was zoomed as well
        el.layout.datarevision++; // needs to change for react method to show data changes
        Plotly.react(el, el.data, el.layout);
      });
    }
    "

plot_exception <-function(
  ...,
  sep=" ",
  type=c("message","warning","cat","print", "none"),
  color="auto",
  console=TRUE,
  size = 6){      
  type=match.arg(type)
  txt = paste(...,collapse=sep)
  if(console){
    if(type == "message") message(txt)
    if(type == "warning") warning(txt)
    if(type == "cat") cat(txt)
    if(type == "print") print(txt)
  }
  if(color =="auto") color <- if(type == "cat") "black" else "red"
  if(txt == "warning") txt <- paste("warning:",txt)
  print(ggplot2::ggplot() +
          ggplot2::geom_text(ggplot2::aes(x=0,y=0,label=txt),color=color,size=size) + 
          ggplot2::theme_void())
  invisible(NULL)
}


## datatable helper

list_funds <- function(data, filter = F, searching = F, fontsize = "100%",
                       visible_cols = c("Name", "ISIN", "Symbol", "Issuer", "Trading currency", "Management fee", "Investment region"),
                       selected = NULL) {
  
  if (filter) {
    filter <- "top"
  } else {
    filter <- "none"
  }

  data %>% 
    datatable(filter = filter, 
              options = list(pageLength = 10, autoWidth = F, searching = searching,  
                             columnDefs = list(list(targets = c(0, which(!(names(.) %in% visible_cols))), 
                                                    visible = F)),
                             buttons = c('colvis'), dom = 'Bfritp',
                             search = list(search = 'CHF'),
                             displayStart = max(selected-10, 0)), 
              selection = list(mode = "single", target = "row", selected = selected), #"single", 
              class = 'compact cell-border') %>%
      formatString(suffix = "%", columns = "Management fee") %>% 
      formatStyle(columns = 1:37, fontSize = fontsize)
}

calc_drawdown <- function(Ra) {
  # same calculation formula as PerformanceAnalytics::Drawdowns geometric = FALSE
  
  p <- 1 + cumsum(Ra)
  m <- cummax(c(1, p))[-1]
  d <- p/m - 1
  
  return(d)
}


  
  
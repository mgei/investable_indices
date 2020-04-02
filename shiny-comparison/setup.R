# options(shiny.reactlog = TRUE)

## packages ----
library(shiny)
# library(argonR)
# library(argonDash)
library(shinydashboard)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(shinyWidgets)
# library(shinysky)
library(plotly)
library(httr)
library(readxl)
library(rvest)
library(DT)
library(magrittr)
library(scales)



## functions ----

get_prices_cache <- function(symbol, 
                             from = (floor_date(Sys.Date() - period(10, units = "years"), "month") - 1), 
                             to = (floor_date(Sys.Date(), "month") - 1),
                             cache_dir = "data/cache/") {
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
  suppressWarnings(out <- tq_get(x = symbol, from = from, to = to, complete_cases = T, warnings = F))
  
  if (!is_tibble(out)) {
    return(NA)
    stop("no data available")
  }
  
  out <- out %>% 
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
  download.file(url, destfile = "data/scrapedpage.html", quiet=TRUE)
  
  D1 <- read_html("data/scrapedpage.html") %>% 
    html_nodes("table.table-grid") %>%
    html_nodes("td") %>%
    html_text()
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
  
  if (paste0(symbol, ".SW", ".RDS") %in% list.files(cache_dir)) {
    cached <- readRDS(paste0(cache_dir, symbol, ".SW", ".RDS"))
    
    if (cached$loaddate + period(reload_if_older_than) >= Sys.Date()) {
      out <- cached$data
      
      return(out)
    }
  }
  
  # get from SIX
  suppressWarnings(out <- get_holdings(paste0(symbol, ".SW")))
  
  if (!is_tibble(out)) {
    return(NA)
    stop("no data available")
  }
  
  # save to cached files
  list(loaddate = Sys.Date(),
       data = out) %>% 
    saveRDS(paste0(cache_dir, symbol, ".SW", ".RDS"))
  
  return(out)
  
}

reload_fundlist <- function() {
  url <- "https://www.six-group.com/exchanges/funds/explorer_export_en.xls"
  GET(url, write_disk("data/fundlist.xls", overwrite = T))
  fundlist <- read_xls("data/temp.xls", skip = 4)
  
  fundlist <- fundlist %>% 
    mutate(`Management fee` = as.double(`Management fee`),
           `Mgmt fee` = `Management fee`/100)
  
  fundlist %>% saveRDS("data/fundlist.RDS")
  
  return(fundlist)
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

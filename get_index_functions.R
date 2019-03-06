library(tidyverse)
library(readxl)
library(httr)
library(tidyquant)
require(R.utils)
library(openxlsx)
library(stringr)
library(RSelenium)
library(Quandl)
library(lubridate)

get_index <- function(index, source, name = index, col = "", file = "") {
       if (source == "MSCI") get_msci(index, name)
  else if (source == "SPind") get_spind(index, col)
  else if (source == "BNP") get_bnp(index)
  else if (source == "FTSE") get_ftse(index, col)
  else if (source == "SIX") get_six(index, col)
  else if (source == "SIX2") get_six2(index, col)
  else if (source == "SIX3") get_six3(index, col)
  else if (source == "SIX4") get_six4(index, col)
  else if (source == "Vienna") get_vienna(index)
  else if (source == "Yahoo") get_yahoo(index) 
  else if (source == "IHS") get_ihs(index, name) 
  else if (source == "Quandl") get_qdl(index, col)
  else if (source == "STOXX") get_stxx(index, col)
  else if (source == "Ossiam") get_osam(index)
  else if (source == "ICE") get_ice(index)
  else if (source == "MSCI_dl") get_msci_dl(file)
  else if (source == "HFR_dl") get_hfr_dl(file, col)
  else if (source == "DL") get_dl(file)
  else if (source == "PIM CO") get_pmc(file)
  else if (source == "DL_xl") get_dl_xl(file)
  else warning("Unknown source.") 
}

lookup_index <- function(indexname) {
  # should refer to a lookup table and return the index we need.
  return("Function to be implemented.")
}

get_msci <- function(index, name) {
  url <- paste0("http://www.msci.com/webapp/indexperf/charts?indices=",
                index,
                ",V,38&startDate=31%20Dec,%201987&priceLevel=0&currency=15&frequency=M&scope=R&format=XLS&baseValue=false&site=gimi")
  
  GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
  data <- read_excel(tf, skip = 6, n_max = nrow(read_excel(tf))-22)
  
  return(data %>% mutate(Date = as.Date(Date)))
}

get_spind <- function(index, col) {
  url <- paste0("https://us.spindices.com/idsexport/file.xls?",
                "hostIdentifier=48190c8c-42c4-46af-8d1a-0cd5db894797&selectedModule=PerformanceGraphView&",
                "selectedSubModule=Graph&yearFlag=tenYearFlag&indexId=",
                index)
  
  GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
  data <- read_excel(tf, skip = 6, n_max = nrow(read_excel(tf))-56) %>% 
    rename(Date = `Effective date`) %>% 
    mutate(Date = as.Date(Date))
  #unlink(tf)
  
  if (col != "") {
    data <- data %>% select(Date, paste(col))
  }
  
  
  return(data)
}

get_bnp <- function(index) {
  
  if (!exists("bnp_auth") || !bnp_auth) {
    WAIT = 5
    SF = paste0(getwd(), "/temp")
    if (!file.exists(SF)) { dir.create(file.path(SF)) }
    ff64 = "/usr/bin/firefox"
    eCap1 <- list(`moz:firefoxOptions` = list(binary = ff64), pageLoadStrategy = 'none', timeouts = list(pageLoad = 10))
    #timeouts = list(script = 5, pageLoad = 10))
    eCap2 <- makeFirefoxProfile(list("browser.download.panel.shown" = FALSE,
                                     "browser.download.manager.showWhenStarting" =  FALSE,
                                     "browser.download.dir" = SF,
                                     "browser.download.folderList" = 2L,
                                     "browser.download.manager.closeWhenDone" = TRUE,
                                     "browser.download.manager.showAlertOnComplete" = FALSE,
                                     "browser.download.animateNotifications" = FALSE,
                                     "browser.helperApps.neverAsk.saveToDisk" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    ))
    
    rS = rsDriver(browser = "firefox", port = 4566L, extraCapabilities = c(eCap1, eCap2))
    rDr <- rS[['client']]
    
    #rDr$open()
    #rDr$close()
    #rS$server$stop()
    #rDr$navigate("about:config")
    
    # go disclamer page
    rDr$navigate(url = "https://indx.bnpparibas.com/PreDisclaimer/Index")
    Sys.sleep(WAIT)
    
    # pass first disclamer
    cb1 <- rDr$findElement(using = "xpath", "//label[@for='checkbox_1']")
    cb3 <- rDr$findElement(using = "xpath", "//label[@for='checkbox_3']")
    cb1$clickElement()
    cb3$clickElement()
    vb <-  rDr$findElement(using = "xpath", "//a[@ng-click='validatePreDisclaimer()']")
    vb$clickElement()
    Sys.sleep(WAIT)
    
    # pass second disclamer
    cb1 <- rDr$findElement(using = "xpath", "//label[@for='checkbox_1']")
    cbr <- rDr$findElement(using = "xpath", "//label[@for='chkRem']")
    cb1$clickElement()
    cbr$clickElement()
    bv <- rDr$findElement(using = "xpath", "//button[@id='btnValidate']")
    bv$clickElement()
    Sys.sleep(WAIT)
    
    rDr$close()
    rS$server$stop()
  }
  
  # whole file
  #idf <- read.xlsx("Indices.xlsx")
  
  ### testing BNP:
  #bnp <- idf[which(str_detect(idf$Index, "^BNP")),]
  
  url <- paste0("https://indx.bnpparibas.com/Strategy/Index?pid=",
                index)
  rDr$navigate(url)
  Sys.sleep(WAIT)
  exp <- rDr$findElement(using = "xpath", "//a[@id='export-history-excel']")
  hrf = exp$getElementAttribute(attrName = 'href')
  
  try(rDr$navigate(hrf[[1]]))
  Sys.sleep(WAIT)
  
  #renameFile(paste0(SF,"/Index.xlsx"), paste0(SF,"/",bnp$Symbol[i],".xlsx"), overwrite = T)
  
  data <- read_excel(paste0(SF,"/Index.xlsx"), range = "A6:B9999") %>% 
    mutate(Date = as.Date(Date, format = "%Y/%m/%d"), Values = as.numeric(Values))
  
  file.remove(paste0(SF,"/Index.xlsx"))
  return(data)
} 
# end of get_bnp()

get_ftse <- function(index, col) {
  url <- paste0("https://www.ftse.com/analytics/factsheets/Home/DownloadHistoricIndex/?indexdetails=", 
                index)
  
  GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  data <- read_excel(tf, range = "B9:D100") %>% filter(!is.na(Date)) %>% 
    select(Date, "Total Return")
  
  if (ncol(data) > 2) {
    data <- data %>% select(Date, "Total Return")
  }
  
  return(data %>% mutate(Date = as.Date(Date)))
}

get_six <- function(index, col) {
  url <- paste0("https://www.six-swiss-exchange.com/downloads/indexdata/",
                index,
                ".csv")
  
  #GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
  GET(url, write_disk("temp.csv", overwrite = T))
  data <- read_csv2("temp.csv", skip = 1) 
  
  data <- data[-1:-4,] %>% 
    rename(Date = "SYMBOL") %>% 
    mutate(Date = as.Date(Date, "%d.%m.%Y")) %>% 
    mutate_if(is.character, funs(as.numeric(.))) %>% 
    select(Date, col)
  
  return(data)
}

get_six2 <- function(index, col) {
  url <- paste0("https://www.six-swiss-exchange.com/downloads/indexdata/",
                index,
                ".csv")
  
  #GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
  GET(url, write_disk("temp.csv", overwrite = T))
  data <- read_csv2("temp.csv", skip = 2) 
  
  data <- data[-1:-4,] %>% 
    rename(Date = "SYMBOL") %>% 
    mutate(Date = as.Date(Date, "%d.%m.%Y")) %>% 
    mutate_if(is.character, funs(as.numeric(.))) %>% 
    select(Date, col)
  
  return(data)
}

get_six3 <- function(index, col) {
  url <- paste0("https://www.six-swiss-exchange.com/downloads/indexdata/",
                index,
                ".csv")
  
  #GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
  GET(url, write_disk("temp.csv", overwrite = T))
  data <- read_csv2("temp.csv", skip = 2) 
  
  data <- data[-1:-4,] %>% 
    rename(Date = "SYMBOL") %>% 
    mutate(Date = as.Date(Date, "%d.%m.%Y")) %>% 
    mutate_if(is.character, funs(as.numeric(.))) %>% 
    select(Date, col)
  
  return(data)
}

get_six4 <- function(index, col) {
  url <- paste0("https://www.six-swiss-exchange.com/downloads/indexdata/",
                index,
                ".csv")
  
  #GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
  GET(url, write_disk("temp.csv", overwrite = T))
  data <- read_csv2("temp.csv", skip = 4)
  
  #colnames(data) <- data[1,] %>% paste()
  data <- data[,c(1,7)]
  colnames(data) <- c("Date", "value")
  
  data <- data %>% 
    filter(Date != "Date") %>% 
    mutate(Date = as.Date(Date, "%d.%m.%Y"),
           value = as.double(value))
  
  return(data)
}

get_vienna <- function(index) {
  # index <- "AT0000726476"
  url <- paste0("https://www.wienerborse.at/indizes/aktuelle-indexwerte/historische-daten/?ID_NOTATION=5687516&ISIN=",
                index,
                "&c7012%5BDATETIME_TZ_START_RANGE%5D=23.07.1850&c7012%5BDATETIME_TZ_END_RANGE%5D=",
                format(Sys.Date(), "%d.%m.%Y"),
                "&c7012%5BDOWNLOAD%5D=csv")
  
  GET(url, write_disk("temp.csv", overwrite = T))
  data <- read_csv2("temp.csv") %>% 
    mutate(Date = dmy(Datum), value = Schlusspreis) %>% 
    select(Date, value)
  
  return(data)  
}

get_yahoo <- function(index) {
  data <- tq_get(index, from = "1950-01-01", freq = "D") %>% select(Date = date, adjusted)
  return(data)
}

get_ihs <- function(index) {
  return("The function to get index data from IHS Markit is not working yet.")
}


get_qdl <- function(index, col) {
  if (is.null(Quandl.api_key())) {
    if (exists(qdl_key)) {
      Quandl.api_key(qld_key) }
    else { 
      return("Please assign your Quandl API key with Quandl.api_key(qld_key) and try again") }
  }
  
  data <- Quandl(index, start_date='1950-01-01') %>% as_tibble() %>% select(Date = contains("Date"), col)
  return(data)
}

get_stxx <- function(index, col) {
  url <- paste0("https://www.stoxx.com/document/Indices/Current/HistoricalData/", index, ".txt")
  
  GET(url, write_disk("temp.csv", overwrite = T))
  data <- read_delim("temp.csv", delim = ";", skip = 2) %>% select(Date = DATE, col) %>% 
    mutate(Date = dmy(Date))
  return(data)
}

get_osam <- function(index) {
  url <- paste0("https://www.ossiam.com/produits/historiquenav/id/", index)
  
  GET(url, write_disk("temp.csv", overwrite = T))
  data <- read_delim("temp.csv", delim = ";")
  return(data)
}

get_ice <- function(index) {
  url <- paste0("https://www.theice.com/publicdocs/data/", index,
                "-monthly-time-series.xlsx")
  
  GET(url, write_disk("temp.csv", overwrite = T))
  data <- read_excel("temp.csv", skip = 10) %>% select(Date = Effective_Date, "Total Return Index Level") %>% 
    mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30"))
  return(data)
}

get_msci_dl <- function(file) {
  data <- read_excel(paste0("./data/", file), skip = 6) %>% 
    mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>% 
    filter(!is.na(Date))
  return(data)
}

get_hfr_dl <- function(file, col) {
  data <- read_delim(paste0("./data/", file), delim = ";", skip = 2, col_names = c("Date", "Index", "Symbol", "return", "price")) %>% 
    mutate(Date = mdy(Date)) %>% 
    filter(Symbol == col) %>% 
    select(Date, price)
  
  return(data)
}

get_dl <- function(file) {
  data <- read_csv(paste0("./data/", file)) %>%
    mutate(Date = dmy(Datum)) %>% 
    select(Date, value = Zuletzt)
  return(data)
}

get_pmc <- function(file) {
  data <- read_excel(paste0("./data/", file)) %>% 
    mutate(Date = as.Date(Date)) %>%
    select(Date, value = `Index Level`)
  return(data)
}

get_dl_xl <- function(file) {
  data <- read_excel(paste0("./data/", file)) %>% 
    mutate(Date = as.Date(`Price Date`)) %>% 
    select(Date, contains("Index"))
  
  return(data)
}
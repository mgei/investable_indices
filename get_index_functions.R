library(tidyverse)
library(readxl)
library(httr)
library(tidyquant)
require(R.utils)
library(openxlsx)
library(stringr)
library(RSelenium)

get_index <- function(index, source) {
  if (source == "msci") get_msci(index)
  else if (source == "spind") get_spind(index)
  else if (source == "bnp") get_bnp(index)
  else if (source == "ftse") get_ftse(index)
  else if (source == "six") get_six(index)
  else if (source == "vienna") get_vienna(index)
  else if (source == "yahoo") get_yahoo(index) 
  else warning("Unknown source.") 
}

lookup_index <- function(indexname) {
  # should refer to a lookup table and return the index we need.
  return("Function to be implemented.")
}

get_msci <- function(index) {
  url <- paste0("http://www.msci.com/webapp/indexperf/charts?indices=",
                index,
                ",V,38&startDate=31%20Dec,%201987&priceLevel=0&currency=15&frequency=M&scope=R&format=XLS&baseValue=false&site=gimi")
  
  GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
  data <- read_excel(tf, skip = 6, n_max = nrow(read_excel(tf))-22)
  #unlink(tf)
  
  return(data %>% mutate(Date = as.Date(Date)))
}

get_spind <- function(index) {
  url <- paste0("https://us.spindices.com/idsexport/file.xls?",
                "hostIdentifier=48190c8c-42c4-46af-8d1a-0cd5db894797&selectedModule=PerformanceGraphView&",
                "selectedSubModule=Graph&yearFlag=tenYearFlag&indexId=",
                index)
  
  GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
  data <- read_excel(tf, skip = 6, n_max = nrow(read_excel(tf))-56)
  #unlink(tf)
  
  return(data %>% 
           rename(Date = `Effective date`) %>% 
           mutate(Date = as.Date(Date)))
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

get_ftse <- function(index) {
  url <- paste0("https://www.ftse.com/analytics/factsheets/Home/DownloadHistoricIndex/?indexdetails=", 
  index)
  
  GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
  data <- read_excel(tf, range = "B9:D100") %>% filter(!is.na(Date))
  
  return(data %>% mutate(Date = as.Date(Date)))
}

get_six <- function(index) {
  url <- paste0("https://www.six-swiss-exchange.com/downloads/indexdata/",
                index,
                ".csv")
  
  GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
  data <- read_csv2(tf, skip = 1) 
  
  data <- data[-1:-4,] %>% 
    rename(Date = "SYMBOL") %>% 
    mutate(Date = as.Date(Date, "%d.%m.%Y")) %>% 
    mutate_if(is.character, funs(as.numeric(.)))
  
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
  data <- read_csv2(tf, skip = 1) 
  
  return("The function to get index data from Vienna Exchange is not working yet.")  
}

get_yahoo <- function(index) {
  data <- tq_get(index, from = "1950-01-01", freq = "M") %>% select(Data = date, adjusted)
  return(data)
}

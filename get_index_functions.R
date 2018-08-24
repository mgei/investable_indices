library(tidyverse)
library(readxl)
library(httr)
library(tidyquant)

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
  url <- "https://indx.bnpparibas.com/Strategy/GetSubProductHistoryData?pid=QcOTMXRY%2B%2BQZaJffHqb7ng%3D%3D&subid=pzPRdySHK5jPSdIkqDQF3A%3D%3D"
  
  GET(url, write_disk("temp.xls", overwrite = T))
  data <- read_excel("temp.xls")
  
  return("The function to get index data from BNP Paribas is not working yet.")
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

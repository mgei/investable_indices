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

# Setup ----

# data download functions are stored in separate file
source("get_index_functions.R")

# Quandl key, login to your 
qdl_key <- read_file("quandlkey.private")
Quandl.api_key(qdl_key)


#
Basiswerte <- read_excel("Basiswerte.xlsx")
r <- nrow(Basiswerte)

for (row in 1:r) {
  if (is.na(Basiswerte[row, "Source"] %>% pull())) {
    next
  }
  
  print(row)
  print(Basiswerte[row, "Basiswert"] %>% pull())
  
  filepath <- paste0("./mydata/", Basiswerte[row, "Basiswert"] %>% pull(), ".csv")
  
  if(!file.exists(filepath)) {
    print("downloading and putting in folder")
    data <- get_index(index = Basiswerte[row, "index"] %>% pull(), 
                      source = Basiswerte[row, "Source"] %>% pull(), 
                      col = Basiswerte[row, "col"] %>% pull(), 
                      file = Basiswerte[row, "file"] %>% pull())
    
    data %>% write_csv(filepath)
  }
  else { print("already in folder")}
}



# explored_etfs <- read_excel("../../SIX/explorer_export_de.xls", skip = 4)
# 
# explored_etfs %>% filter(Produkttyp == "Exchange Traded Funds") %>% mutate(Verwaltungsgebühr = as.double(Verwaltungsgebühr)) %>% 
#   group_by(Basiswert) %>% 
#   summarise(n = n(), 
#             meanGebuehr = mean(`Verwaltungsgebühr`, na.rm = T),
#             meanSpread = mean(Spread, na.rm = T)) %>% arrange(desc(n)) %>% 
#   ungroup() -> Basiswerte_1
# 
# Basiswerte_1 %>% #left_join(indices, by = c("Basiswert" = "Index")) %>%
#   write.xlsx("yolo.xlsx")


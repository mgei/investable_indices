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


# foreign exchange rates
# currencies <- Basiswerte %>% 
#   filter(!is.na(Source), Source != "BNP") %>% 
#   select(Currency) %>% 
#   distinct() %>% 
#   pull()

# https://data.snb.ch/en/topics/ziredev#!/cube/devkum?fromDate=2000-01&toDate=2019-08&dimSel=D0(M1),D1(EUR1,GBP1,USD1,BRL100,ZAR1,JPY100,HKD100)
forex <- read_csv2("snb-data-devkum-en-selection-20190902_1430.csv", skip = 3) 

forex <- forex %>% 
  select(1, 3, 4) %>% 
  mutate(Value = as.double(Value),
         Date = ymd(str_c(Date, "-01")) + months(1) - days(),
         D1 = str_remove_all(D1, "[0-9]")) %>% 
  rename(Currency = D1) %>% 
  mutate(mt = month(Date),
         yr = year(Date))

#
Basiswerte <- read_excel("Basiswerte.xlsx")
r <- nrow(Basiswerte)

indexdata <- tibble()
indexdata_chf <- tibble()

for (row in 1:r) {
  source <- Basiswerte[row, "Source"] %>% pull()
  if (is.na(source) | source == "BNP") {
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
  else { 
    print("already in folder")
    data <- read_csv(filepath)

  }
  
  data <- data %>% 
    rename(Date = 1, adjusted = 2) %>% 
    filter(adjusted > 0)
  
  indexdata <- bind_rows(indexdata,
                         data %>% mutate(Basiswert = pull(Basiswerte[row, "Basiswert"])))
  
  
  if (pull(Basiswerte[row, "Currency"]) == "CHF") {
    data_chf <- data
  } else {
    data_chf <- data %>% mutate(mt = month(Date),
                                yr = year(Date)) %>% 
      left_join(forex %>% 
                  filter(Currency == pull(Basiswerte[row, "Currency"])) %>% 
                  select(-Date), by = c("mt", "yr")) %>% 
      mutate(adjusted = adjusted*Value) %>% 
      select(Date, adjusted)
  }
  
  indexdata_chf <- bind_rows(indexdata_chf,
                             data_chf%>% mutate(Basiswert = pull(Basiswerte[row, "Basiswert"])))

}

indexdata_chf %>% 
  filter(!is.na(adjusted), year(Date) >= 2000) %>% 
  group_by(Basiswert) %>% 
  tq_transmute(select = adjusted, mutate_fun = to.monthly, indexAt = "lastof") -> yolo

yolo %>% 
  mutate(cumret = adjusted/first(adjusted)-1) %>% 
  filter(Date == max(Date)) %>% 
  arrange((cumret))

indexdata_chf %>% filter(Basiswert == "SMIM Swiss Market Index Mid (TR)") %>% 
  tail()

  
yolo %>% 
  mutate(cumret = adjusted/first(adjusted)-1) %>% 
  ggplot(aes(x = Date, y = cumret, color = Basiswert)) + 
  geom_line() +
  theme(legend.position = "none")



for (row in 1:r) {
  Basiswerte

  
  }
Basiswerte




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


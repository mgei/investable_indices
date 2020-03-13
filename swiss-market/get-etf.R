library(tidyverse)
library(tidyquant)
library(readxl)

etfs <- read_excel("explorer_export_en-etfs.xls", skip = 4)

data <- tibble()
for (i in 10:nrow(etfs)) { # nrow(stocks)
  
  print(paste(i, etfs[i,"Symbol"] %>% pull()))
  
  temp <- tq_get(etfs[i,"Symbol"] %>% pull() %>% paste0(".SW"), from = "2000-01-01")
  
  if (!is.tibble(temp)) {
    next
    # print("yolo")
  }
  
  temp %>% write_csv(paste0("data-etf/",
                            etfs[i,"Symbol"] %>% pull(),
                            ".csv"))
  
  data <- data %>% 
    bind_rows(temp %>% mutate(Company = etfs[i,"Name"] %>% pull(),
                              Symbol = etfs[i,"Symbol"] %>% pull()))
  
  Sys.sleep(1)
}

data <- data %>% 
  filter(!is.na(adjusted))
  

data %>% saveRDS("etfs.RDS")


etfs <- etfs %>% 
  left_join(data %>% distinct(Symbol) %>% mutate(historic = T), by = "Symbol")



x <- tq_get(etfs[2,"Symbol"] %>% pull() %>% paste0(".SW"), from = "2000-01-01")



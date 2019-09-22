library(tidyverse)

library(httr)
# library(textreadr)
library(rvest)
# library(magrittr)

url <- "https://etfdb.com/screener/#page="
pages <- 1:5 #1:92

etfs <- tibble()
for (page in pages) {
  print(str_c("page ", page))
  
  temp_etfs <- read_html(str_c(url, page)) %>% 
    html_table(header = T) %>% .[1] %>% 
    data.frame() %>% 
    as_tibble()
  
  etfs <- bind_rows(etfs, temp_etfs)
}

colnames(etfs)  <- c("Symbol", "ETF Name", "Previous Closing Price", "Total Assets ($MM)", "Avg. Volume", "YTD", "Overall Rating", "Asset Class")

etfs <- etfs %>% 
  filter(Symbol != "Export to CSV with ETFdb.com Pro") %>% 
  mutate(`Previous Closing Price` = str_remove(`Previous Closing Price`, "\\$") %>% as.double(),
         `Total Assets ($MM)` = str_remove_all(`Total Assets ($MM)`, "\\$|,") %>% as.double(),
         `Avg. Volume` = str_remove_all(`Avg. Volume`, "\\$|,") %>% as.double(),
         YTD = str_remove_all(YTD, "\\%|,") %>% as.double() %>% divide_by(100)) %>% 
  select(-`Overall Rating`) %>% 
  distinct()

etfs %>% saveRDS("etfs.RDS")
  
etfs %>% 
  group_by(`Asset Class`) %>% 
  summarise(assets = sum(`Total Assets ($MM)`)) %>% 
  ggplot(aes(x = reorder(`Asset Class`, desc(assets)), y = assets, fill = `Asset Class`)) + 
  geom_col(alpha = 0.8) +
  geom_label(aes(label = scales::number(assets))) +
  scale_y_continuous(labels = scales::number, breaks = seq(0, 200000000, by = 25000000)) +
  labs(title = "ETF total assets by asset class",
       x = "asset class",
       y = "total assets ($MM)",
       caption = "data: etfdb.com") +
  theme(legend.position = "none")

etfs %>% 
  ggplot(aes(y = YTD, x = `Asset Class`)) +
  geom_boxplot() +
  geom_point(aes(color = `Asset Class`))

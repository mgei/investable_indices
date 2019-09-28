library(tidyverse)

library(httr)
# library(textreadr)
library(rvest)
# library(magrittr)
library("RColorBrewer")


# url <- "https://etfdb.com/screener/#page="
# pages <- 1:5 #1:92
# 
# etfs <- tibble()
# for (page in pages) {
#   print(str_c("page ", page))
#   
#   temp_etfs <- read_html(str_c(url, page)) %>% 
#     html_table(header = T) %>% .[1] %>% 
#     data.frame() %>% 
#     as_tibble()
#   
#   etfs <- bind_rows(etfs, temp_etfs)
# }
# 
# colnames(etfs)  <- c("Symbol", "ETF Name", "Previous Closing Price", "Total Assets ($MM)", "Avg. Volume", "YTD", "Overall Rating", "Asset Class")
# 
# etfs <- etfs %>% 
#   filter(Symbol != "Export to CSV with ETFdb.com Pro") %>% 
#   mutate(`Previous Closing Price` = str_remove(`Previous Closing Price`, "\\$") %>% as.double(),
#          `Total Assets ($MM)` = str_remove_all(`Total Assets ($MM)`, "\\$|,") %>% as.double(),
#          `Avg. Volume` = str_remove_all(`Avg. Volume`, "\\$|,") %>% as.double(),
#          YTD = str_remove_all(YTD, "\\%|,") %>% as.double() %>% divide_by(100)) %>% 
#   select(-`Overall Rating`) %>% 
#   distinct()
# 
# etfs %>% saveRDS("etfs.RDS")
#   
# etfs %>% 
#   group_by(`Asset Class`) %>% 
#   summarise(assets = sum(`Total Assets ($MM)`)) %>% 
#   ggplot(aes(x = reorder(`Asset Class`, desc(assets)), y = assets, fill = `Asset Class`)) + 
#   geom_col(alpha = 0.8) +
#   geom_label(aes(label = scales::number(assets))) +
#   scale_y_continuous(labels = scales::number, breaks = seq(0, 200000000, by = 25000000)) +
#   labs(title = "ETF total assets by asset class",
#        x = "asset class",
#        y = "total assets ($MM)",
#        caption = "data: etfdb.com") +
#   theme(legend.position = "none")
# 
# etfs %>% 
#   ggplot(aes(y = YTD, x = `Asset Class`)) +
#   geom_boxplot() +
#   geom_point(aes(color = `Asset Class`))



# # ETFDB API
# url <- "https://etfdb.com/api/screener/"
# 
# response <- GET(url,
#                 add_headers("Content-Type" = "application/json",
#                             page = 2,
#                             per_page = 25,
#                             sort_by = "ytd",
#                             sort_direction = "desc",
#                             only =  c("meta", "data")))
# 
# response
# 
# POST(url, )


etfs <- read_csv2("output.csv2") %>% 
  mutate_all(str_trim) %>% 
  mutate(etf.assets = etf.assets %>% str_remove_all("\\$|,") %>% as.double(),
         etf.average_volume = etf.average_volume %>% str_remove_all("\\$|,") %>% as.double(),
         etf.ytd = etf.ytd %>% str_remove_all("%") %>% as.double() %>% divide_by(100))

# mark LETFs
etfs %>% 
  filter(!is.na(etf.ytd)) %>% 
  mutate(leverage = str_detect(etf.name.text, "2x|3x|2X|3X|leverage|Leverage|inverse|Inverse|UltraPro|Ultra"),
         color = if_else(leverage, "AAA", etf.asset_class)) %>% 
  ggplot(aes(x = etf.asset_class, y = etf.ytd)) +
  geom_point(aes(color = color, size = etf.assets), alpha = 0.5, position = position_jitter(w = 0.3, h = 0)) +
  geom_boxplot(outlier.shape = NA, alpha = 0) +
  geom_hline(yintercept = 0, color = "red", size = 0.2) +
  scale_size_continuous(range = c(0.5, 5)) +
  scale_color_manual(values = c("grey", RColorBrewer::brewer.pal(9, "Set1"))) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-1,2,0.1)) +
  labs(x = "", y = "2019 YTD",
       title = "Performance of ETFs by class in 2019",
       subtitle = "by 26 September 2019",
       caption = "data: etfdb.com") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


RColorBrewer::display.brewer.all()
display.brewer.pal(n = 8, name = 'RdBu')



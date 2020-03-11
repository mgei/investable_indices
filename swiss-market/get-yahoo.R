library(tidyverse)
library(tidyquant)
library(readxl)

stocks <- read_excel("explorer_export_en.xls", skip = 4)

data <- tibble()
for (i in 224:nrow(stocks)) { # nrow(stocks)
  
  print(paste(i, stocks[i,"Symbol"] %>% pull()))
  
  temp <- tq_get(stocks[i,"Symbol"] %>% pull() %>% paste0(".SW"), from = "2000-01-01")
  
  temp %>% write_csv(paste0("data/",
                            stocks[i,"Symbol"] %>% pull(),
                            ".csv"))
  
  data <- data %>% 
    bind_rows(temp %>% mutate(Company = stocks[i,"Company"] %>% pull(),
                              Symbol = stocks[i,"Symbol"] %>% pull()))
  
  Sys.sleep(1)
}

data_backup <- data

data %>% distinct()

data %>% 
  group_by(Symbol) %>% 
  summarise(date = min(date)) %>% 
  ggplot(aes(x = date)) +
  geom_histogram()


tq_get()

data %>% 
  filter(date >= Sys.Date() - period(1, "month"),) %>% 
  group_by(Symbol, Company) %>% 
  mutate(return30 = adjusted/first(adjusted)-1) %>% 
  filter(date == max(date)) %>%
  ungroup() %>% 
  filter(row_number() %in% sample(1:nrow(.), 20, replace = F)) %>% 
  # arrange(-return30)
  ggplot(aes(y = return30, x = reorder(Company, return30))) +
  geom_col() +
  geom_label(aes(y = 0, label = Company), size = 2) +
  coord_flip() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())
  
  
  ggplot(aes(x = 1, y = return30)) +
  geom_boxplot()

  
url <- paste0("https://www.six-group.com/exchanges/shares/info_details_en.html?id=", stocks[1, "ISIN"] %>% pull() , "CHF4", "&portalSegment=EQ") 

stocks %>% group_by(`Index member`) %>% count()


library(rvest)
url <- paste0("https://www.six-group.com/exchanges/shares/info_details_en.html?id=", stocks[1, "ISIN"] %>% pull() , "CHF4", "&portalSegment=EQ") 

html <- read_html(url)
cast <- html_nodes(html, ".plaintable:nth-child(3) td:nth-child(1) :nth-child(2) .last")
html_text(cast, trim = TRUE)


shares_issued <- tibble()
for (i in 1:nrow(stocks)) {
  
  print(paste(i, stocks[i, "Symbol"] %>% pull()))
  
  url <- paste0("https://www.six-group.com/exchanges/shares/info_details_en.html?id=", stocks[i, "ISIN"] %>% pull() , "CHF4", "&portalSegment=EQ")
  html <- read_html(url)
  cast <- html_nodes(html, ".plaintable:nth-child(3) td:nth-child(1) :nth-child(2) .last")
  issued_shares <-html_text(cast, trim = TRUE)
  
  shares_issued <- shares_issued %>% 
    bind_rows(tibble(ISIN = stocks[i, "ISIN"] %>% pull(),
                     shares_issued = issued_shares))
}

shares_issued <- shares_issued %>% 
  mutate(shares_issued_int = as.double(str_remove_all(shares_issued, "'")))

stocks <- stocks %>% 
  left_join(shares_issued %>% select(ISIN, shares_issued = shares_issued_int), by = "ISIN")

stocks %>% saveRDS("stocks.RDS")

data %>% 
  group_by(Symbol) %>% 
  filter()

tidyquant::tq_get_options()

# NESNE.SW
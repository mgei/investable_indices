library(tidyverse)
library(magrittr)

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
etfs %<>% 
  mutate(leverage = case_when(str_detect(etf.name.text, "Short-Term|Short Term|Short Duration|Short Maturity|Short Treasury") ~ F,
                              str_detect(etf.name.text, "2x|3x|2X|3X|4x|4X|leverage|Leverage|inverse|Inverse|UltraPro|Ultra") ~ T,
                              str_detect(etf.name.text, "Short|short") ~ T,
                              T ~ F))
  
  
etfs %>% 
  filter(!is.na(etf.ytd)) %>% 
  mutate(color = if_else(leverage, "AAA", etf.asset_class)) %>% 
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

# only get symbols that are unleveraged and have more than 100 million USD AUM
symbols <- etfs %>% 
  filter(!leverage,
         etf.assets > 100) %>% 
  pull(etf.symbol.text)


library(tidyquant)

# download and save each to csv
if (!file.exists("pricedata")) {
  dir.create("pricedata")
}

for (symbol in symbols) {
  print(str_c(which(symbols == symbol), " ", symbol))
  assign(symbol, tq_get(symbol, get  = "stock.prices", from = "2000-01-01", to = Sys.Date()))
  
  write_csv(get(symbol), str_c("pricedata/", symbol, ".csv"))
  
  rm(list=symbol)
  print("done")
}

directory <- list.files("pricedata/.")

# directory %>% length()
# symbols %>% length()

# empty tibble for prices, needs the date column
pricedata <- tibble(date = as.Date("1900-01-01")) %>% 
  filter(date != as.Date("1900-01-01"))

for (file in directory) {
  print(paste(which(directory == file), file))
  # make it monthly
  temp <- read_csv(paste0("pricedata/", file), col_types = "Ddddddd") %>% 
    tq_transmute(select     = open:adjusted, 
                 mutate_fun = to.period, 
                 period     = "months") %>% 
    mutate(date = ceiling_date(date, unit = "months") - days(1)) %>% 
    select(date, !!str_remove(file, ".csv") := adjusted)

  pricedata %<>% full_join(temp, by = "date")
  rm(temp)
  
  print("done")
}

file <- "SPY.csv"

# just verify that all are end of month dates and no confusion
pricedata %>% 
  select(date) %>% 
  arrange(date) %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(month, year) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

library(ggrepel)

pricedata %>% 
  arrange(date) %>% 
  gather(symbol, price, -date) %>%
  filter(!is.na(price)) %>% 
  group_by(symbol) %>% 
  summarise(first = min(date), last = max(date), obs = n()) %>% 
  left_join(select(etfs, symbol = etf.symbol.text, etf.asset_class, etf.assets), by = "symbol") %>% 
  ggplot(aes(x = reorder(symbol, desc(first)), y = first, ymin = first, ymax = last, color = etf.asset_class)) + 
  geom_linerange(alpha = 0.8) + 
  geom_text_repel(aes(label = if_else(symbol %in% c("SPY", "GLD", "SLV", "VXX", "VNQ", "USO", "BND", "LQD", "MNA", "AOK", "FXE", "VV0", "BNDX", "DGRO"), 
                                      symbol, NA_character_), hjust = 1), 
                  nudge_y = -100, direction = "y",
                  color = "black", size = 3) +
  scale_y_date(limits = c(as.Date("1999-01-01"), (Sys.Date() + 40))) +
  scale_color_manual(values = RColorBrewer::brewer.pal(9, "Set1")) +
  coord_flip() +
  theme_bw() + 
  labs(y = "Availability", x = "", color = "", 
       title = "Historical availability of ETFs",
       caption = "Data: etfdb.com, finance.yahoo.com") +
  theme(#axis.text=element_text(size=4),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  guides(colour=guide_legend(override.aes=list(size=4)))

pricedata %>% arrange(date)

periods_from <- seq(as.Date("2000-01-31"), as.Date("2019-01-31"), by = "years")
periods_to   <- periods_from + years(1) - months(1)

for (y in 2000:2019-1999) {
  
  pricedata %>% 
    arrange(date) %>% 
    gather(symbol, price, -date) %>%
    filter(!is.na(price)) %>% 
    group_by(symbol) %>% 
    filter(min(date) <= periods_from[y]) %>% 
    tq_transmute(select     = price, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 type       = "arithmetic") %>% 
    ungroup() %>% 
    spread(symbol, monthly.returns) %>% 
    filter(between(date, periods_from[y], periods_to[y])) -> yearsdata
  
  yearsdata_matrix <- yearsdata %>% 
    select(-date) %>% 
    as.matrix() 
  
  yearsdata_matrix[is.na(yearsdata_matrix)] <- 0
  
  yearsdata_matrix %>% 
    prcomp(scale. = T, center = T) -> pca
  
  pca %>% summary()
  
  pca$rotation %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(PC = 1:nrow(.)) %>% 
    select(PC, everything()) %>% 
    gather(symbol, loading, -PC) %>% 
    group_by(PC) %>% 
    filter(loading == max(loading)) %>% 
    arrange(PC) %>% 
    ungroup() -> toploadings
  
  weights <- toploadings %>% 
    distinct(symbol) %>% 
    .[1:6,] %>% 
    mutate(weight = 1/nrow(.)) %>% 
    select(symbol, weight)
  
  
  pricedata %>% 
    arrange(date) %>% 
    gather(symbol, price, -date) %>%
    filter(!is.na(price)) %>% 
    group_by(symbol) %>% 
    filter(min(date) <= periods_from[y+1]) %>% 
    tq_transmute(select     = price, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 type       = "arithmetic") %>% 
    ungroup() %>% 
    spread(symbol, monthly.returns) %>% 
    filter(between(date, periods_from[y+1], periods_to[y+1])) %>% 
    gather(symbol, return, -date) -> yearsdata2
  
  yearsdata2 %>% 
    mutate(return = if_else(is.na(return), 0, return)) %>% 
    tq_portfolio(assets_col  = symbol, 
                 returns_col = return, 
                 weights     = weights, 
                 col_rename  = "return",
                 rebalance_on = "years") -> portfolio
  
  portfolio %>% 
    left_join(pricedata %>% 
                select(date, SPY) %>% 
                arrange(date) %>% 
                tq_transmute(select     = SPY, 
                             mutate_fun = periodReturn, 
                             period     = "monthly", 
                             type       = "arithmetic"),
              by = "date") %>% 
    rename(SPY = monthly.returns) %>% 
    gather(pf, return, -date) %>% 
    group_by(pf) %>% 
    tq_performance(Ra = return, Rb = NULL, performance_fun = SharpeRatio) %>% print()
  
  print("pf:")
  portfolio %>% 
    mutate(r = return + 1) %>% 
    pull(r) %>% 
    prod() %>% sum(-1) %>% print()
  
  print("spy:")
  pricedata %>% 
    select(date, SPY) %>% 
    arrange(date) %>% 
    tq_transmute(select     = SPY, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 type       = "arithmetic") %>% 
    filter(between(date, periods_from[y+1], periods_to[y+1])) %>% 
    mutate(r = monthly.returns + 1) %>% 
    pull(r) %>% 
    prod() %>% sum(-1) %>% print()

}

# portfolio optimization ----
pricedata %>% 
  arrange(date) %>% 
  gather(symbol, price, -date) %>%
  filter(!is.na(price)) %>% 
  group_by(symbol) %>% 
  filter(min(date) <= periods_from[y]) %>% 
  tq_transmute(select     = price, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic") %>% 
  ungroup() %>% 
  rename(return = monthly.returns) -> rets

library(PortfolioAnalytics)
library(timetk)

rets_tk <- rets %>%
  select(symbol, date, return) %>%
  spread(symbol, return) %>%
  tk_xts(silent = TRUE)

stocks.selection <- tibble(symbol = c("SPY", "GLD", "BND", "LQD", "FXE", "SLV"),
                           sector = c("equities", "metals", "debt", "debt", "currencies", "metals"))

pspec <- portfolio.spec(assets = stocks.selection$symbol,
                        category_labels = stocks.selection$sector)
print(pspec)
str(pspec)

pspec <- add.constraint(portfolio=pspec,
                        type="full_investment")

pspec <- add.constraint(portfolio=pspec,
                        type="box",
                        min=0,
                        max=0.4)

pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)

pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.7)

pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.2)

pspec <- add.constraint(portfolio=pspec, type="return", return_target=0.007)

pspec <- add.constraint(portfolio=pspec, type="leverage_exposure", leverage=1.3)

summary(pspec)
# To get an overview on the specs, their indexnum and whether they are enabled I suggest the follwoing
consts <- plyr::ldply(pspec$constraints, function(x){c(x$type,x$enabled)})
consts
pspec$constraints[[which(consts$V1=="box")]]
pspec <- add.constraint(pspec, type="box",
                        min=0, max=0.5, 
                        indexnum=which(consts$V1=="box"))
pspec$constraints[[which(consts$V1=="box")]]
# to disable constraints
pspec$constraints[[which(consts$V1=="position_limit")]]
pspec <- add.constraint(pspec, type="position_limit", enable=FALSE, # only specify argument if you do enable the constraint
                        indexnum=which(consts$V1=="position_limit"))
pspec$constraints[[which(consts$V1=="position_limit")]]


pspec <- add.objective(portfolio=pspec,
                       type='risk',
                       name='var')
pspec <- add.objective(portfolio=pspec,
                       type='risk',
                       name='ETL',
                       arguments=list(p=0.95),
                       enabled=FALSE)

pspec <- add.objective(portfolio=pspec,
                       type='return',
                       name='mean')
# print(pspec)

pspec <- add.objective(portfolio=pspec, 
                       type="risk_budget",
                       name="var",
                       max_prisk=0.3)
pspec <- add.objective(portfolio=pspec, 
                       type="risk_budget",
                       name="ETL",
                       arguments=list(p=0.95),
                       max_prisk=0.3,
                       enabled=FALSE)
# for an equal risk contribution portfolio, set min_concentration=TRUE
pspec <- add.objective(portfolio=pspec, 
                       type="risk_budget",
                       name="ETL",
                       arguments=list(p=0.95),
                       min_concentration=TRUE,
                       enabled=FALSE)
print(pspec)


pspec <- portfolio.spec(assets = stocks.selection$symbol,
                        category_labels = stocks.selection$sector)
pspec <- add.constraint(portfolio=pspec,
                        type="full_investment") # weights sum to 1
p <- add.constraint(portfolio=pspec,
                    type="box",
                    min = -0.5,
                    max = +0.5)
# to create the efficient frontier 
pspec <- add.objective(portfolio=pspec, 
                       type="return",
                       name="mean") # mean
pspec <- add.objective(portfolio=pspec, 
                       type="risk",
                       name="var") # uses sd
meansd.ef <- create.EfficientFrontier(
  R = rets,
  portfolio = pspec,
  type = "mean-sd",
  n.portfolios = 25,
)
# summary(meansd.ef, digits=2) # to print the whole efficient frontier
# meansd.ef$frontier[1:2,] # shows the first two portfolios
chart.EfficientFrontier(meansd.ef,
                        match.col="StdDev", # which column to use for risk
                        type="l", 
                        RAR.text="Sharpe Ratio",
                        tangent.line = FALSE,
                        chart.assets=TRUE,
                        labels.assets=TRUE,xlim=c(0.03,0.20),ylim=c(0,0.055))
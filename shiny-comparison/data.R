# data

fundlist <- readRDS("data/fundlist_plus.RDS")

fund_options_labels <- c("nur CHF", "nur vorhandene Daten", "ETFs", "Replikation physisch", "mit Dividenden")

fund_categories_labels <- c("Schweiz", "Global", "Dividend", "Value", "Growth", "Momentum", "Tech", "Emerging Markets", "Obligationen", "Gold",
                            "Rohstoffe", "Immobilien", "diverse Themen", "diverse Strategien")

# fundlist_schweiz <- fundlist %>% 
#   filter(`Investment region` == "Switzerland", 
#          prices_nrow > 1,
#          str_detect(`Asset class`, "Equity"))
# 
# fundlist_global <- fundlist %>%
#   filter(`Investment region` == "Global", 
#          prices_nrow > 1,
#          str_detect(`Asset class`, "Equity"))
# 
# fundlist_dividend <- fundlist %>%
#   filter(str_detect(Name, "Dividend|DIVIDEND|dividend"),
#          prices_nrow > 1,
#          dividends_nrow > 0,
#          str_detect(`Asset class`, "Equity"))
# 
# fundlist_value <- fundlist %>% 
#   filter(str_detect(Name, "Value|VALUE|value"),
#          prices_nrow > 1,
#          str_detect(`Asset class`, "Equity"))
# 
# fundlist_growth <- fundlist %>% 
#   filter(str_detect(Name, "Growth|GROWTH|growth"),
#          prices_nrow > 1,
#          str_detect(`Asset class`, "Equity"))
# 
# fundlist_momentum <- fundlist %>% 
#   filter(str_detect(Name, "Momentum|MOMENTUM|momentum"),
#          prices_nrow > 1,
#          str_detect(`Asset class`, "Equity"))
# 
# fundlist_tech <- fundlist %>% 
#   filter(str_detect(Name, "Tech|TECH|tech"),
#          prices_nrow > 1,
#          str_detect(`Asset class`, "Equity"))
# 
# fundlist_defence <- fundlist %>% 
#   filter(str_detect(Name, "Defence|DEFENCE"),
#          prices_nrow > 1,
#          str_detect(`Asset class`, "Equity"))
# 
# fundlist_emerging <- fundlist %>% 
#   filter(`Investment region` == "Emerging Markets",
#          prices_nrow > 1,
#          str_detect(`Asset class`, "Equity"))
# 
# fundlist_fixedincome <- fundlist %>% 
#   filter(`Asset class` == "Fixed Income",
#          prices_nrow > 1)
# 
# fundlist_gold <- fundlist %>% 
#   filter(str_detect(Name, "Gold|GOLD|gold"),
#          `Asset class` == "Commodities",
#          prices_nrow > 1)
# 
# fundlist_commodities <- fundlist %>% 
#   filter(`Asset class` == "Commodities",
#          prices_nrow > 1)
# 
# 
#   
# tabPanel("Obligationen", dataTableOutput("funds_list_fixedincome", width = "100%")),
# tabPanel("Gold", dataTableOutput("funds_list_gold", width = "100%")),
# tabPanel("Rohstoffe", dataTableOutput("funds_list_commodities", width = "100%")),
# tabPanel("diverse Themes", dataTableOutput("funds_list_themes", width = "100%")),
# tabPanel("diverse Strategien", dataTableOutput("funds_list_strategies", width = "100%"))
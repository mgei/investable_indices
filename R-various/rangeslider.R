# Using ggplotly rangeslider for interactive relative performance (stock returns)
# https://stackoverflow.com/questions/56551892/using-ggplotly-rangeslider-for-interactive-relative-performance-stock-returns

library(plotly)
library(tidyquant)

# stocks <- tq_get(c("AAPL", "MSFT"), from = "2019-01-01")
# stocks %>% saveRDS("data/stocks.RDS")

stocks <- readRDS("data/stocks.RDS")
range_from <- as.Date("2019-02-01")

stocks_range <- stocks %>% 
  filter(date >= range_from) %>% 
  group_by(symbol) %>% 
  mutate(performance = adjusted/first(adjusted)-1)

p <- stocks_range %>% 
  ggplot(aes(x = date, y = performance, color = symbol)) +
  geom_line()

ggplotly(p, dynamicTicks = T) %>%
  rangeslider(borderwidth = 1) %>%
  layout(hovermode = "x", yaxis = list(tickformat = "%"))

library(tidyquant)
library(tidyverse)

spx <- tq_get("^GSPC", from = "1950-01-01")

spx %>% 
  mutate(r = adjusted/lag(adjusted) - 1,
         rr = r/lag(r) - 1,
         rrr = rr/lag(rr) - 1) %>% 
  select(date, adjusted, r, rr, rrr) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~name, ncol = 1, scales = "free_y")


# invest at once or over n months?
spx_r <- spx %>% 
  tq_transmute(Ra = adjusted, mutate_fun = monthlyReturn) %>% 
  mutate(performance = cumprod(1 + monthly.returns) - 1)


library(PerformanceAnalytics)

xts_to_tibble <- function(x) {
  as_tibble(x) %>% 
    mutate(date = index(x)) %>% 
    select(date, everything()) %>% 
    return()
}

tibble_to_xts <- function(x) {
  xts(x = x %>% select(-date),
      order.by = x$date)
}


for (i in seq(from = 12*50, to = nrow(spx)-1, by = 12)) {
  weights1 <- rep(1, length(spx_r$monthly.returns))
  weights1[0:i] <- 0
  weights1[i:(i+11)] <- seq(from = 1, to = 12, by = 1)/12
  weights1 <- spx_r %>% 
    select(date) %>% 
    mutate(SPX = weights1)
  
  weights2 <- rep(1, length(spx_r$monthly.returns))
  weights2[0:i] <- 0
  weights2 <- spx_r %>% 
    select(date) %>% 
    mutate(SPX = weights2)
  
  
  pf1 <- Return.portfolio(R = tibble_to_xts(spx_r %>% select(date, SPX = monthly.returns)), 
                          weights = tibble_to_xts(weights1), 
                          verbose = T, wealth.index = T)
  
  pf2 <- Return.portfolio(R = tibble_to_xts(spx_r %>% select(date, SPX = monthly.returns)), 
                          weights = tibble_to_xts(weights2), 
                          verbose = T, wealth.index = T)
  
  plot <- pf1$wealthindex %>% 
    xts_to_tibble() %>% 
    mutate(invest = "gradually") %>% 
    bind_rows(pf2$wealthindex %>% 
                xts_to_tibble() %>% 
                mutate(invest = "at once")) %>% 
    ggplot(aes(x = date, y = portfolio.wealthindex, color = invest)) +
    geom_line() +
    scale_y_log10()
  
  print(plot)
  
  readline(prompt="Press [enter] to continue")
}



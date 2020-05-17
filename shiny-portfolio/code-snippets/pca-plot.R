rets_raw <- get_yahoo_prices_cache(c("SPY", "GLD", "BND")) %>% 
  filter(year(date) >= 2010) %>% 
  group_by(symbol) %>% 
  mutate(ret = adjusted/lag(adjusted)-1) %>% 
  select(symbol, date, ret) %>% 
  filter(!is.na(ret)) %>% 
  pivot_wider(names_from = "symbol", values_from = "ret")

library(ggfortify)

pca_res <- prcomp(rets_raw %>% select(-date), scale. = TRUE)

autoplot(pca_res, data = rets_raw,
         size = 0.2,
         alpha = 0.2,
         loadings = TRUE, 
         loadings.colour = "red",
         loadings.label = TRUE, 
         loadings.label.size = 3,
         loadings.label.repel = T) +
  theme_bw()

autoplot(pca_res)

autoplot(pca_res, data = rets_raw)


library(tidyverse)
library(tidyquant)

rets <- FANG %>% 
  group_by(symbol) %>% 
  mutate(ret = adjusted/lag(adjusted)-1) %>% 
  select(symbol, date, ret) %>% 
  pivot_wider(names_from = "symbol", values_from = ret)
 
# weights <- c(0.4, 0.1, 0.25, 0.25)
# weights <- rep(0.25,4)

threshold <- 0.05

r_out <- tibble()
i0 <- 1
trade_rebalance <- 1
pf_value <- 1
for (i in 1:nrow(rets)) {
  r <- rets[i0:i,]
  
  # https://stackoverflow.com/questions/53288100/pass-function-arguments-by-column-position-to-mutate-at
  j <- 0
  r_i <- r %>% 
    mutate_if(is.numeric, replace_na, 0) %>%
    mutate_if(is.numeric, list(v = ~ pf_value * weights[j <<- j + 1] * cumprod(1 + .))) %>%
    mutate(pf = rowSums(select(., contains("_v")))) %>% 
    mutate_at(vars(ends_with("_v")), list(w = ~ ./pf))
  
  touch_upper_band <- any(r_i[nrow(r_i),] %>% select(ends_with("_w")) %>% unlist() > weights + threshold)
  touch_lower_band <- any(r_i[nrow(r_i),] %>% select(ends_with("_w")) %>% unlist() < weights - threshold)
  
  if (touch_upper_band | touch_lower_band | i == nrow(rets)) {
    i0 <- i + 1
    r_out <- bind_rows(r_out, r_i %>% mutate(trade_rebalance = trade_rebalance))
    pf_value <- r_i[[nrow(r_i), "pf"]]
    trade_rebalance <- trade_rebalance + 1
  }
}

r_out %>% head()
# # A tibble: 6 x 15
# date             FB      AMZN     NFLX      GOOG  FB_v AMZN_v NFLX_v GOOG_v    pf FB_v_w AMZN_v_w NFLX_v_w GOOG_v_w trade_rebalance
# <date>        <dbl>     <dbl>    <dbl>     <dbl> <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl>    <dbl>    <dbl>    <dbl>           <dbl>
#   1 2013-01-02  0        0         0        0        0.25   0.25   0.25   0.25   1     0.25     0.25     0.25     0.25                1
# 2 2013-01-03 -0.00821  0.00455   0.0498   0.000581 0.248  0.251  0.262  0.250  1.01  0.245    0.248    0.259    0.247               1
# 3 2013-01-04  0.0356   0.00259  -0.00632  0.0198   0.257  0.252  0.261  0.255  1.02  0.251    0.246    0.255    0.249               1
# 4 2013-01-07  0.0229   0.0359    0.0335  -0.00436  0.263  0.261  0.270  0.254  1.05  0.251    0.249    0.257    0.243               1
# 5 2013-01-08 -0.0122  -0.00775  -0.0206  -0.00197  0.259  0.259  0.264  0.253  1.04  0.251    0.250    0.255    0.245               1
# 6 2013-01-09  0.0526  -0.000113 -0.0129   0.00657  0.273  0.259  0.261  0.255  1.05  0.261    0.247    0.249    0.244               1

r_out %>% tail()
# # A tibble: 6 x 15
# date             FB      AMZN       NFLX     GOOG  FB_v AMZN_v NFLX_v GOOG_v    pf FB_v_w AMZN_v_w NFLX_v_w GOOG_v_w trade_rebalance
# <date>        <dbl>     <dbl>      <dbl>    <dbl> <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl>    <dbl>    <dbl>    <dbl>           <dbl>
#   1 2016-12-22 -0.0138  -0.00553  -0.00727   -0.00415 0.945   1.10   1.32   1.08  4.45  0.213    0.247    0.297    0.243              10
# 2 2016-12-23 -0.00111 -0.00750   0.0000796 -0.00171 0.944   1.09   1.32   1.08  4.43  0.213    0.246    0.298    0.243              10
# 3 2016-12-27  0.00631  0.0142    0.0220     0.00208 0.950   1.11   1.35   1.08  4.49  0.212    0.247    0.301    0.241              10
# 4 2016-12-28 -0.00924  0.000946 -0.0192    -0.00821 1.11    1.12   1.10   1.11  4.45  0.250    0.252    0.247    0.250              11
# 5 2016-12-29 -0.00488 -0.00904  -0.00445   -0.00288 1.11    1.11   1.10   1.11  4.42  0.250    0.252    0.248    0.251              11
# 6 2016-12-30 -0.0112  -0.0200   -0.0122    -0.0140  1.09    1.09   1.08   1.09  4.36  0.251    0.250    0.248    0.251              11

r_out %>% 
  mutate(performance = pf-1) %>% 
  ggplot(aes(x = date, y = performance)) +
  geom_line(data = FANG %>% 
              group_by(symbol) %>% 
              mutate(performance = adjusted/adjusted[1L]-1),
            aes(color = symbol)) +
  geom_line(size = 1)

r_out %>% 
  select(date, ends_with("_w")) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line()



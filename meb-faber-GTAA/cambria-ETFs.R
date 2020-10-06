library(tidyverse)
library(tidyquant)



tickers <- read_delim("TAIL	Cambria Tail Risk ETF	Multi-Asset	$401.96	11.09%	333,729	$21.39	-1.25%	
GVAL	Cambria Global Value ETF	Equity	$93.16	-26.18%	18,064	$17.33	1.17%	
SYLD	Cambria Shareholder Yield ETF	Equity	$67.15	-8.92%	5,803	$36.01	2.42%	
GMOM	Cambria Global Momentum ETF	Multi-Asset	$66.42	-5.12%	9,909	$24.19	0.75%	
GAA	Cambria Global Asset Allocation ETF	Multi-Asset	$47.64	-1.54%	7,152	$26.53	-0.19%	
TRTY	Cambria Trinity ETF	Multi-Asset	$38.11	-6.94%	4,580	$22.61	0.67%	
EYLD	Cambria Emerging Shareholder Yield ETF	Equity	$33.24	-10.51%	2,509	$28.20	0.82%	
SOVB	Cambria Sovereign High Yield Bond ETF	Bond	$22.16	-1.65%	1,217	$24.58	1.07%	
FYLD	Cambria Foreign Shareholder Yield ETF	Equity	$19.07	-13.38%	1,956	$20.32	0.84%	
TOKE	Cambria Cannabis ETF	Equity	$11.42	-22.71%	8,624	$11.20	2.75%	
VAMO	Cambria Value and Momentum ETF	Alternatives	$10.75	-9.90%	1,186	$18.44	0.99%	
BLDG	Cambria Global Real Estate ETF	Real Estate	$2.57	N/A	N/A	N/A	0.00% ", delim = "\t", col_names = F) %>% 
  select(1, 2)


etfs <- tickers %>% 
  bind_rows(tibble(X1 = "SPY", X2 = "SPY")) %>% 
  mutate(prices = map(X1, ~.x %>% tq_get()))

etf_returns <- etfs %>% 
  unnest(prices) %>% 
  group_by(X1) %>% 
  tq_transmute(adjusted, monthlyReturn) %>% 
  ungroup()

etf_performance <- etfs %>% 
  unnest(prices) %>% 
  filter(year(date) >= 2019) %>% 
  group_by(X1) %>% 
  tq_transmute(adjusted, dailyReturn) %>% 
  mutate(performance = cumprod(daily.returns + 1) - 1)

etf_performance %>%
  ggplot(aes(x = date, y = performance, color = X1)) +
  geom_line(aes(linetype = X1 == "SPY")) +
  theme(legend.position = "bottom")

etf_returns %>% 
  group_by(X1, year = year(date)) %>%
  filter(n() == 12 | year == 2020) %>% 
  tq_performance(Ra = monthly.returns, performance_fun = table.AnnualizedReturns) %>% 
  select(X1, year, SR = `AnnualizedSharpe(Rf=0%)`) %>%
  ungroup() %>% 
  pivot_wider(names_from = X1, values_from = SR) %>% 
  arrange(year)

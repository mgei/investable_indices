library(tidyverse)
library(tidyquant)

sp500 <- tq_get("^GSPC", from = "1990-01-01")
sp500_tr <- tq_get("^SP500TR", from = "1990-01-01")

spx <- left_join(sp500 %>% select(date, SP500 = adjusted), 
                 sp500_tr %>% select(date, SP500_TR = adjusted), 
                 by = "date")

spx_dividend <- spx %>% 
  mutate(SP500_return = SP500/lag(SP500)-1,
         SP500_TR_predividend = lag(SP500_TR)*(1+SP500_return),
         dividend = SP500_TR - SP500_TR_predividend)

spx_dividend_yield <- spx %>% 
  group_by(year = year(date)) %>% 
  summarise(dividend = sum(dividend, na.rm = T),
            SP500_TR = mean(SP500_TR)) %>% 
  mutate(dividendyield = dividend/SP500_TR)


spx %>% 
  mutate(SP500_return = SP500/lag(SP500)-1,
         SP500_TR_predividend = lag(SP500_TR)*(1+SP500_return),
         dividend = SP500_TR - SP500_TR_predividend) %>% 
  group_by(year = year(date)) %>% 
  summarise(dividend = sum(dividend), SP500 = mean(SP500)) %>% 
  mutate(yield = dividend/SP500) %>% 
  ggplot(aes(x = year, y = yield)) +
  geom_col()


spx %>% 
  mutate(SP500_return = SP500/lag(SP500)-1,
         SP500_TR_predividend = lag(SP500_TR)*(1+SP500_return),
         dividend = SP500_TR - SP500_TR_predividend) %>% 
  group_by(year = year(date)) %>% 
  summarise(dividend = sum(dividend), SP500 = mean(SP500), SP500_TR = mean(SP500_TR)) %>% 
  mutate(yield = dividend/SP500_TR) %>% 
  ggplot(aes(x = year, y = yield)) +
  geom_col()
  
smi %>% 
  arrange(date) %>% 
  mutate(SMI_return = SMI/lag(SMI)-1,
         SMIC_predividend = lag(SMIC)*(1+SMI_return),
         dividend = SMIC - SMIC_predividend) %>% 
  group_by(year = year(date)) %>% 
  summarise(dividend = sum(dividend), SMI = mean(SMI), SMIC = mean(SMIC)) %>% 
  mutate(yield = dividend/SMIC) %>% 
  ggplot(aes(x = year, y = yield)) +
  geom_col()


spx %>% 
  group_by(year = year(date)) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  mutate(yield = ((SP500_TR- lag(SP500_TR)) - (SP500 - lag(SP500)))/SP500_TR) %>% 
  print(n = 100)
  ggplot(aes(x = date, y = yield)) +
  geom_col()

  

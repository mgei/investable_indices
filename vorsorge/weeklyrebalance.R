library(tidyverse)
library(lubridate)
library(tidyquant)
library(RcppRoll)

source("reload-data.R")

prices <- readRDS(paste0("data/",
                         list.files("data") %>% sort() %>% tail(1))) 


prices



# 12.08.20 145.15
# 14.07.20 144.57
# 12.06.20 141.08
# 13.05.20 139.77
# 15.04.20 136.79
# 10.03.20 138.25
# 11.02.20 149.51

prices %>% filter(Date == dmy("10.02.20")) %>% 
  glimpse()

prices %>% tail()

fridays <- tibble(Date = seq(from = min(prices$Date), to = max(prices$Date), by = "days")) %>% 
  mutate(weekday = wday(Date)) %>% 
  filter(weekday == 6) %>% 
  mutate(friday = T)

returns <- prices %>% 
  mutate(DateTrade = Date+1) %>% 
  full_join(fridays, by = c("DateTrade" = "Date")) %>% 
  arrange(DateTrade) %>% 
  mutate(`Net Asset Value` = na.locf(`Net Asset Value`, na.rm = F)) %>% 
  filter(friday) %>% 
  select(DateTrade, NAV = `Net Asset Value`) %>% 
  mutate(r = NAV/lag(NAV)-1,
         rr = r/lag(r)-1,
         rr_a = r/abs(lag(r)) -1,
         rr_aa = abs(r)/abs(lag(r)) -1,
         p = cumprod(1+replace(r, is.na(r), 0))-1,
         vola4 = RcppRoll::roll_sd(r, n = 4, fill = NA, align = "right", na.rm = T),
         vola6 = RcppRoll::roll_sd(r, n = 6, fill = NA, align = "right", na.rm = T),
         vola8 = RcppRoll::roll_sd(r, n = 8, fill = NA, align = "right", na.rm = T),
         vola10 = RcppRoll::roll_sd(r, n = 10, fill = NA, align = "right", na.rm = T),
         vola12 = RcppRoll::roll_sd(r, n = 12, fill = NA, align = "right", na.rm = T),
         ma4 = RcppRoll::roll_mean(p, n = 4, fill = NA, align = "right", na.rm = T),
         ma40 = RcppRoll::roll_mean(p, n = 40, fill = NA, align = "right", na.rm = T),
         r_forward = lead(r))

returns %>% 
  ggplot(aes(x = DateTrade, y = r)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent)

returns %>% 
  ggplot(aes(x = p/ma4, y = r_forward, color = r_forward > 0)) +
  geom_point(size = 0.2) +
  # geom_smooth(size = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  # scale_y_log10() +
  # scale_x_log10() +
  theme_bw()




library(tidyverse)
library(lubridate)


# Migrosbank Funds

funds <- tribble(
  ~Name, ~NAV, ~R1Y, ~R3Y, ~Valor, ~ISIN,
  "Migros Bank (CH) Fonds 0 V",	"CHF	103.60", "+3.22%",	"n.v.", 36569669, "CH0365696696",
  "Migros Bank (CH) Fonds 25 V", "CHF	124.69", "+6.39%",	"+10.88%", 2340649, "CH0023406496",
  "Migros Bank (CH) Fonds 45 V", "CHF	113.09", "+9.33%",	"n.v.", 2340656, "CH0023406561",
  "Migros Bank (CH) Fonds Sustainable 0 V", "CHF	103.02", "+2.84%", "n.v.", 36569685, "CH0365696852",
  "Migros Bank (CH) Fonds Sustainable 25 V", "CHF	108.83", "+6.65%", "n.v.", 36569690, "CH0365696902",
  "Migros Bank (CH) Fonds Sustainable 45 V", "CHF	149.33", "+10.45%", "+17.35", 10270610, "CH0102706105"
) %>% 
  select(-NAV, -R1Y, -R3Y) %>% 
  mutate(link = c("https://www.swissfunddata.ch/sfdpub/de/funds/excelData/79080",
                  "https://www.swissfunddata.ch/sfdpub/de/funds/excelData/23137",
                  "https://www.swissfunddata.ch/sfdpub/de/funds/excelData/79260",
                  "https://www.swissfunddata.ch/sfdpub/de/funds/excelData/79087",
                  "https://www.swissfunddata.ch/sfdpub/de/funds/excelData/79090",
                  "https://www.swissfunddata.ch/sfdpub/de/funds/excelData/32033"))
i <- 1

prices <- tibble()
for (i in 1:nrow(funds)) {
  p <- read_delim(funds[i, "link"] %>% pull(), delim = ";", skip = 2, locale=locale(decimal_mark = ".")) %>% 
    select(-`Issue Price`, -`Redemption Price`, -`Closing Price`) %>% 
    mutate(ISIN = funds[i, "ISIN"] %>% pull())
  
  prices <- bind_rows(prices, p)
}
rm(p)

prices <- left_join(prices,
                    funds %>% select(-link), 
                    by = "ISIN")

prices %>% 
  group_by(Name) %>% 
  summarise(min_date = min(Date))


prices %>% 
  saveRDS("prices.RDS")

prices %>% 
  group_by(Name) %>% 
  mutate(p = )


prices %>% 
  filter(Date == max(Date))


prices %>% 
  select(Date, Name, `Chart Price`) %>% 
  group_by(Name) %>% 
  mutate(performance = `Chart Price`/`Chart Price`[1L] - 1) %>% 
  ggplot(aes(x = Date, y = performance, color = Name)) +
  geom_line()


prices %>% 
  filter(Name == "Migros Bank (CH) Fonds Sustainable 45 V") %>% 
  group_by(floor_date(Date, unit = "months")) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  mutate(return = `Chart Price`/lag(`Chart Price`)-1) %>% 
  rowwise() %>% 
  mutate(return_plus = max(0, return)) %>% 
  ungroup() %>% 
  filter(!is.na(return), !is.na(return_plus)) %>% 
  group_by(year = year(Date)) %>%
  mutate(performance = cumprod(return + 1) -1,
         performance_plus = cumprod(return_plus + 1) - 1) %>% 
  ungroup() %>%
  select(Date, performance, performance_plus) %>% 
  pivot_longer(cols = -Date) %>% 
  mutate(year = year(Date)) %>% 
  ggplot(aes(x = Date, y = value, color = name)) +
  geom_line() +
  facet_wrap(~year, scales = "free_x")


prices %>% 
  filter(Name == "Migros Bank (CH) Fonds Sustainable 45 V") %>% 
  group_by(floor_date(Date, unit = "months")) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  mutate(return = `Chart Price`/lag(`Chart Price`)-1) %>% 
  mutate(invested = `Chart Price`>lag(`Chart Price`, 2)) %>% 
  tail(n = 20)
  
  mutate(return_invested = return*invested) %>% 
  filter(!is.na(return), !is.na(return_invested)) %>% 
  group_by(year = year(Date)) %>%
  mutate(performance = cumprod(return + 1) -1,
         performance_invested = cumprod(return_invested + 1) - 1) %>% 
  ungroup() %>%
  select(Date, performance, performance_invested) %>% 
  pivot_longer(cols = -Date) %>% 
  mutate(year = year(Date)) %>% 
  ggplot(aes(x = Date, y = value, color = name)) +
  geom_line() +
  facet_wrap(~year, scales = "free_x")

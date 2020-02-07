library(tidyverse)
library(scales)

# Migrosbank Funds

funds <- tribble(
  ~Name, ~NAV, ~R1Y, ~R3Y, ~Valor, ~ISIN,
  "Migros Bank (CH) Fonds 0 V",	"CHF	103.60", "+3.22%",	"n.v.", 36569669, "CH0365696696",
  "Migros Bank (CH) Fonds 25 V", "CHF	124.69", "+6.39%",	"+10.88%", 2340649, "CH0023406496",
  "Migros Bank (CH) Fonds 45 V", "CHF	113.09", "+9.33%",	"n.v.", 2340656, "CH0023406561",
  "Migros Bank (CH) Fonds Sustainable 0 V", "CHF	103.02", "+2.84%", "n.v.", 36569685, "CH0365696852",
  "Migros Bank (CH) Fonds Sustainable 25 V", "CHF	108.83", "+6.65%", "n.v.", 36569690, "CH0365696902",
  "Migros Bank (CH) Fonds Sustainable 45 V", "CHF	149.33", "+10.45%", "+17.35", 10270610, "CH0102706105"
)

# https://www.swissfunddata.ch/sfdpub/anlagefonds

pricedata <- tibble()
for (fund in 1:nrow(funds)) {
  val <- pull(funds[fund, "Valor"])
  
  temp <- read_delim(paste0("returns/", val, ".csv"), delim = ";", skip = 2, locale=locale(decimal_mark = ",")) %>% 
    mutate(Valor = val)
  
  pricedata <- bind_rows(pricedata, temp)
}

pricedata %>% summary()

val <- unique(pricedata$Valor)[4]

pricedata %>% 
  select(-"Issue Price", -"Redemption Price", -"Closing Price", -starts_with("CCY")) %>% 
  pivot_longer(-c(Valor, Date)) %>%
  # filter(Valor == val) %>% 
  filter(name %in% c("Net Asset Value")) %>% 
  ggplot(aes(x = Date, y = value, color = factor(Valor))) +
  geom_line(aes(linetype = name)) +
  scale_y_continuous(labels = comma)

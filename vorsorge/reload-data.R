print("reloading data")

library(tidyverse)

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



# we only need 45 V
i <- nrow(funds)
prices <- read_delim(funds[i, "link"] %>% pull(), delim = ";", skip = 2, locale=locale(decimal_mark = ".")) %>% 
  select(-`Issue Price`, -`Redemption Price`, -`Closing Price`) %>% 
  mutate(ISIN = funds[i, "ISIN"] %>% pull()) %>% 
  arrange(Date) %>% 
  mutate(r = `Net Asset Value`/lag(`Net Asset Value`)-1,
         r = replace_na(r, 0))


prices %>% saveRDS(paste0("data/", as.character(Sys.Date()), ".RDS"))

print("done")
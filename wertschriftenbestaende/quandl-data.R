library(tidyverse)
library(Quandl)

Quandl.api_key(read_file("../performance-eval/quandl.key"))

# 1
by_securitycat_and_businesssector        <- Quandl("SNB/BAWEBESEC")

#2
by_businessector_currency_issuerdomicile <- Quandl("SNB/BAWEBEDOMSECWA")

#3
by_securitycat_currency_issuerdomicile   <- Quandl("SNB/BAWEBEWA")

#   security_cat business_sector investment_currency issuer_domicile
# 1       X             X 
# 2                     X                 X               X
# 3       X                               X               X

by_securitycat_and_businesssector %>% 
  as_tibble() %>% 
  pivot_longer(cols = -Date) %>% 
  separate(name, info = c(""))

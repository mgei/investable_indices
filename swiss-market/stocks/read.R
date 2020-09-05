library(tidyverse)

stocklist <- read_delim("issuers_all_de.csv", delim = ";", locale = locale(encoding = "windows-1252"))

stocklist %>% 
  filter(`Primär-kotiert`)

stocklist[1,] %>% glimpse()


stocks <- readRDS("../stocks.RDS")

stocks %>% filter(Symbol == "HIAG") %>% glimpse()

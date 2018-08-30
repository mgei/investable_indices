library(tidyverse)
library(readxl)
library(httr)
library(tidyquant)
require(R.utils)
library(openxlsx)
library(stringr)
library(RSelenium)

source("get_index_functions.R")

etf_list <- read_excel("Indices.xlsx")

etf_list[1,] %>% select(index) %>% pull() %>% get_index("Yahoo")

bnp2 <- etf_list[13,] %>% select(index) %>% pull() %>% get_index("BNP")

sp <- etf_list[316,] %>% select(index) %>% pull() %>% get_index("SPind")

ftse <- etf_list[332,] %>% select(index) %>% pull() %>% get_index("FTSE")

msci <- etf_list[651,] %>% select(index) %>% pull() %>% get_index("MSCI")

for (row in nrow(etf_list)) {
  etf_list[row,] %>% select()
}
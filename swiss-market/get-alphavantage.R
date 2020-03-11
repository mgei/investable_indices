library(tidyquant)


key <- read_file("alpha.key") %>% 
  str_remove("\n")


tidyquant::av_api_key(key)


library(alphavantager)


av_get()
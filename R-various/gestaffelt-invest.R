# Einstieg gestaffelt

library(tidyquant)

spy <- tq_get("SPY")

spy_r <- spy %>% 
  mutate(ret = adjusted/lag(adjusted)-1)

dates <- seq(from = min(spy$date), to = max(spy$date), by = "years")
dates <- dates[3]
d <- dates[1]

out <- tibble()
for (d in dates) {
  s1 <- spy_r %>% 
    filter(!is.na(ret),
           date >= d) %>% 
    mutate(performance = cumprod(1+ret)-1,
           entrydate = as.Date(d),
           entrytype = "immediate")
  
  s2 <- spy_r %>% 
    filter(!is.na(ret),
           date >= d) %>%
    mutate(invested = round(row_number()/252*12)/12) %>%
    rowwise() %>% 
    mutate(invested = min(invested, 1)) %>% 
    ungroup() %>% 
    mutate(ret = invested*ret,
           performance = cumprod(1+ret)-1,
           entrydate = as.Date(d),
           entrytype = "12month")
  
  out <- bind_rows(out, s1, s2)
}

out %>% 
  ggplot(aes(x = date, y = performance)) +
  geom_line(aes(color = entrytype), size = 0.2) +
  facet_wrap(~entrydate)
  # theme(legend.position = "right")
  

  
round((1:250/250)*12)

1/12

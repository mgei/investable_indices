library(tidyquant)
library(tidyverse)
library(lubridate)
library(scales)

spx <- tq_get("^GSPC", from = "1900-01-01")

library(elections)
data(eldat)

el_spx <- eldat %>% 
  as_tibble() %>% 
  filter(presel.Date >= min(spx$date)) %>% 
  mutate(spx = map(presel.Date, ~spx %>% 
                     filter(date >= (.x - months(1)), date <= (.x + months(1))) %>% 
                     mutate(performance = adjusted/first(adjusted) - 1)))

el_spx %>% 
  unnest(spx) %>% 
  ggplot(aes(x = date, y = performance)) +
  geom_vline(aes(xintercept = presel.Date, color = winnerparty)) +
  geom_line() +
  labs(x = "", y = "", color = "", title = "Performance of the S&P 500 index around US presidential elections (-/+ 1 month)") +
  scale_color_manual(values = c("blue", "red")) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  facet_wrap(~electionyear, scales = "free_x") +
  theme_bw() +
  theme(legend.position = c(0.8,0.1))

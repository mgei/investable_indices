

fonds <- read_delim("~/Downloads/111563.csv", skip = 2, 
                    delim = ";", locale=locale(decimal_mark = "."))

spi <- read_csv2("https://www.six-group.com/exchanges/downloads/indexdata/hspitr.csv")
  

spi <- spi %>% 
  select(1,2) %>% 
  filter(row_number() >= 5) %>% 
  rename(date = 1, spi = 2) %>% 
  mutate(date = dmy(date), spi = as.double(spi)) %>% 
  arrange(date)


spi_bb <- spi %>% 
  left_join(fonds %>% select(date = Date, bb = `Net Asset Value`),
            by = "date") %>% 
  pivot_longer(cols = -date) %>% 
  filter(date >= fonds$Date %>% min()) %>% 
  arrange(name, date) %>% 
  group_by(name) %>% 
  mutate(r = value/lag(value)-1)

regr_fun <- function(data) {
  coef(lm(bb ~ spi, data = timetk::tk_tbl(data, silent = TRUE)))
}

x <- spi_bb %>% 
  select(-value) %>% 
  pivot_wider(values_from = r, names_from = name) %>% 
  tq_mutate(mutate_fun = rollapply,
            width      = 21,
            FUN        = regr_fun,
            by.column  = FALSE,
            col_rename = c("alpha", "beta"))

x %>% 
  ggplot(aes(x = date, y = alpha)) +
  geom_col()

x %>% 
  ggplot(aes(x = date, y = beta)) +
  geom_col()

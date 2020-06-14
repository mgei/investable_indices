library(tidyverse)
library(readxl)
library(lubridate)

raw <- read_excel("snb-data-bawebedomsecwa-de-all-20200522_0900.xlsx", skip = 24, col_names = F)

classes <- raw %>% 
  head(n=5)


data <- raw %>% 
  tail(nrow(raw)-5)

data_long <- data %>% 
  rename(date = 1) %>% 
  mutate(date = ymd(paste0(date, "-01"))) %>% 
  pivot_longer(cols = -date) %>% 
  arrange(name, date) %>% 
  left_join(classes[1,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[2,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[3,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[4,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[5,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(symbol = value) %>% 
              select(-1),
            by = "name") %>% 
  select(-name) %>% 
  mutate(value = as.double(value))

data_long_clean <- data_long %>% 
  filter(!is.na(value))

data_long_clean %>% 
  summary()

data_long_clean %>% 
  # group_by(`Domizil des Depotinhabers`) %>% 
  # 1 Ausländische Depotinhaber                 15360
  # 2 Inländische Depotinhaber                  41115
  # 3 Inländische und ausländische Depotinhaber 15360
  # group_by(Anlagewährung) %>%
  # 1 Alle Währungen    14367
  # 2 Euro              14367
  # 3 Schweizer Franken 14367
  # 4 Übrige Währungen  14367
  # 5 US-Dollar         14367
  # group_by(`Domizil des Emittenten`) %>%
  # 1 Alle Emittenten          23945
  # 2 Emittenten Ausland       23945
  # 3 Emittenten Inland        23945
  # group_by(Wirtschaftssektor) %>%
  # 1 Institutionelle Anleger - Finanzierungs- und Vermögensverwaltungsinstitutionen - Kollektivanlageinstitutionen  3840
  # 2 Institutionelle Anleger - Finanzierungs- und Vermögensverwaltungsinstitutionen - Total                         3840
  # 3 Institutionelle Anleger - Kredit- und Versicherungshilfstätigkeiten                                            2715
  # 4 Institutionelle Anleger - Sozialversicherungen                                                                 3840
  # 5 Institutionelle Anleger - Total                                                                               11520
  # 6 Institutionelle Anleger - Versicherungen und Pensionskassen - Pensionskassen                                   3840
  # 7 Institutionelle Anleger - Versicherungen und Pensionskassen - Total                                            3840
  # 8 Kommerzielle Kunden - Öffentliche Hand                                                                         3840
  # 9 Kommerzielle Kunden - Total                                                                                   11520
  # 10 Privatkunden                                                                                                  11520
  # 11 Total                                                                                                         11520
  count()

data_long_clean_ch <- data_long_clean %>% 
  # group_by(Wirtschaftssektor) %>% 
  # filter(!(value < 100)) %>% 
  # ungroup() %>% 
  mutate(Wirtschaftssektor = str_replace_all(Wirtschaftssektor, " - ", "\n")) %>% 
  filter(`Domizil des Depotinhabers` == "Inländische Depotinhaber",
         Anlagewährung == "Schweizer Franken",
         `Domizil des Emittenten` == "Alle Emittenten")

data_long_clean_ch %>% 
  ggplot(aes(x = date, y = value, color = Wirtschaftssektor)) +
  geom_line() +
  facet_wrap(~Wirtschaftssektor, scales = "free_y") +
  theme(legend.position = "none")

spi_tr <- read_csv2("https://www.six-group.com/exchanges/downloads/indexdata/hspitr.csv",
                    skip = 2) %>% 
  filter(!(row_number() %in% c(1,2))) %>% 
  rename(date = 1) %>% 
  mutate(date = dmy(date)) %>% 
  mutate_if(is.character, as.double)

spi_tr_clean <- spi_tr %>%
  select(1, 2) %>% 
  rename(SPI_TR = 2) %>% 
  group_by(month = floor_date(date, "months")) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(-month) %>% 
  mutate(date = floor_date(date, "months")) %>% 
  filter(date >= min(data_long_clean$date)) %>% 
  mutate(r = SPI_TR/last(SPI_TR)) 

spi_tr_clean %>% 
  ggplot(aes(x = date, y = r)) +
  geom_line()

spi_pr <- read_csv2("https://www.six-group.com/exchanges/downloads/indexdata/hspipr.csv",
                    skip = 2) %>% 
  filter(!(row_number() %in% c(1,2))) %>% 
  rename(date = 1) %>% 
  mutate(date = dmy(date)) %>% 
  mutate_if(is.character, as.double)

spi_pr_clean <- spi_pr %>%
  select(1, 2) %>% 
  rename(SPI_PR = 2) %>% 
  group_by(month = floor_date(date, "months")) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(-month) %>% 
  mutate(date = floor_date(date, "months")) %>% 
  filter(date >= min(data_long_clean$date)) %>% 
  mutate(r = SPI_PR/last(SPI_PR)) 

spi_pr_clean %>% 
  ggplot(aes(x = date, y = r)) +
  geom_line()


data_long_clean_ch %>% 
  left_join(spi_tr_clean, by = "date") %>%
  rename(MrdCHF = value) %>% 
  filter(!str_detect(Wirtschaftssektor, "Total")) %>% 
  group_by(Wirtschaftssektor) %>% 
  mutate(SPI = first(MrdCHF)*r) %>% 
  select(-r, -SPI_TR) %>% 
  pivot_longer(cols = c(MrdCHF, SPI)) %>% 
  ggplot(aes(x = date, y = value, color = Wirtschaftssektor, group = name, linetype = name == "SPI")) +
  geom_line() +
  # scale_alpha(range = c())
  facet_wrap(~Wirtschaftssektor, scales = "free_y") +
  theme(legend.position = "none")

### data

raw <- read_excel("snb-data-bawebesec-de-all-20200522_0900.xlsx", skip = 38, col_names = F)

classes <- raw %>% 
  head(n=4)

data_cat <- raw %>% 
  tail(nrow(raw)-4)

data_cat_long <- data_cat %>% 
  rename(date = 1) %>% 
  mutate(date = ymd(paste0(date, "-01"))) %>% 
  pivot_longer(cols = -date) %>% 
  arrange(name, date) %>% 
  left_join(classes[1,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[2,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[3,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(!!.[[1,1]] := value) %>% 
              select(-1),
            by = "name") %>% 
  left_join(classes[4,] %>% 
              pivot_longer(cols = -1) %>% 
              rename(symbol = value) %>% 
              select(-1),
            by = "name") %>% 
  select(-name) %>% 
  mutate(value = as.double(value))

data_cat_long_clean <- data_cat_long %>% 
  filter(!is.na(value))


data_cat_long_clean %>% 
  filter(Wertschriftenkategorie == "Aktien") %>% 
  distinct(Wirtschaftssektor)

data_cat_long_clean %>% 
  filter(Wertschriftenkategorie == "Aktien",
         Wirtschaftssektor == "Privatkunden") %>% 
  distinct(`Domizil des Depotinhabers`)

data_cat_long_clean %>% 
  mutate(Wirtschaftssektor = str_replace_all(Wirtschaftssektor, " - ", "\n")) %>% 
  filter(Wertschriftenkategorie == "Aktien", 
         `Domizil des Depotinhabers` == "Inländische Depotinhaber") %>% 
  left_join(spi_pr_clean, by = "date") %>%
  rename(MrdCHF = value) %>% 
  # filter(!str_detect(Wirtschaftssektor, "Total")) %>% 
  filter(str_detect(Wirtschaftssektor,
                    "Alle Sektoren|Kommerzielle Kunden\nTotal|Institutionelle Anleger\nTotal|Privatkunden")) %>%
  group_by(Wirtschaftssektor) %>% 
  mutate(SPI = first(MrdCHF)*r) %>% 
  select(-r, -SPI_PR) %>% 
  pivot_longer(cols = c(MrdCHF, SPI)) %>% 
  ggplot(aes(x = date, y = value, color = Wirtschaftssektor, group = name, linetype = name == "SPI")) +
  geom_line() +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  # scale_alpha(range = c())
  facet_wrap(~Wirtschaftssektor, scales = "free_y") + 
  theme(legend.position = "none") +
  labs(title = "Aktienbestände in Kundendepots per Ende Monat in Milliarden Franken",
       subtitle = "SPI Index als gepunktete Linie",
       y = "Aktienbestand in Mrd. CHF", x = "",
       caption = "Daten: SNB, SIX, Grafik: Martin Geissmann")


data_cat_long_clean %>%
  filter(Wirtschaftssektor != "Alle Sektoren",
         `Domizil des Depotinhabers` != "Inländische und ausländische Depotinhaber") %>% 
  group_by(Wirtschaftssektor, `Domizil des Depotinhabers`) %>% 
  count()


data_cat_long_clean %>%
  filter(Wirtschaftssektor != "Alle Sektoren",
         `Domizil des Depotinhabers` != "Inländische und ausländische Depotinhaber") %>% 
  filter(date == as.Date("2020-03-01")) %>% 
  select(-symbol) %>% 
  filter(Wertschriftenkategorie == "Total") %>% 
  arrange(value) %>% 
  select(-Wertschriftenkategorie)


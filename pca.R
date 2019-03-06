library(tidyverse)
library(readxl)
library(lubridate)

Basiswerte <- read_excel("Basiswerte.xlsx")

Basiswerte <- Basiswerte %>% filter(!is.na(Source))


data <- tibble(Date = seq(ymd("1950-01-01"), ymd("2019-01-31"), by = "day"))

currencies <- read_delim("snb-data-devkum-de-all-20190201_1430 (1).csv", delim = ";", skip = 3) %>% 
  mutate(D1 = str_sub(D1, 1,3), 
         Date = as.Date(ceiling_date(ymd(Date, truncated = 1), "month") - days(1))) %>% 
  filter(D0 == "M1") # M1 is end-of-month (Monatsende), M0 would be monthly mean (Monatsmittel)


currencies %>% filter(D1 %in% (Basiswerte %>% select(Currency) %>% distinct() %>% pull()))

for (row in 1:nrow(Basiswerte)) {
  
  newdata <- read_csv(paste0("mydata/", Basiswerte[row, "Basiswert"], ".csv"), skip = 1,
                      col_types = "Dd",
                      col_names = c("Date", pull(Basiswerte[row, "Basiswert"])))
  
  # verify you have a Date col and two cols overall
  if (!"Date" %in% colnames(newdata)) {
    print(paste(Basiswerte[row, "Basiswert"], "has no column Date"))
    break
  }
  
  if (ncol(newdata) != 2) {
    print(paste(Basiswerte[row, "Basiswert"], "has unequal 2 cols:"))
    print(colnames(newdata) %>% paste(collapse = ", "))
    break
  }
  
  if(sapply(newdata, class)[[1]] != "Date") {
    print("Date is not in date format")
    break
  }
  
  #newdata <- newdata %>% rename(!! pull(Basiswerte[row, "Basiswert"]) := 2)
  
  data <- data %>% left_join(newdata, by = "Date")
}

# data %>% mutate(rs = rowSums(.[2:110], na.rm = TRUE)) %>% 
#   filter(rs > 0) %>% select(-rs) %>% tail()

data <- data %>% fill(everything(), .direction = "down")


eofm <- tibble(Date = seq(ymd("1950-01-01"), ymd("2019-02-01"), by = "month")-days(1))

eofm <- eofm %>% left_join(data, by = "Date")

eofm %>% gather(Basiswert, value, -Date) %>% filter(!is.na(value)) %>% 
  group_by(Basiswert) %>% summarise(first = min(Date), obs = n()) -> dataseries

dataseries %>% mutate(last = today()) %>% gather(point, Date, -Basiswert, -obs) %>% 
  ggplot(aes(x = Date, y = reorder(Basiswert, obs))) + 
  geom_line(aes(group = Basiswert, color = Basiswert)) +
  theme(text = element_text(size=7),
        legend.position = "none") +
  facet_wrap()

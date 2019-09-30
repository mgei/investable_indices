#package import
library(rvest)
library(RSelenium)
library(tidyverse)


###############################################
##########       SCRAP FUNCTION       #########
###############################################

temp_etfs<-function(main_page) {
  temp_etfs <- main_page %>%
    html_table(header = T) %>% .[1] %>%
    data.frame() %>%
    as_tibble()
  ifelse(length(temp_etfs) > 0, temp_etfs<-temp_etfs, temp_etfs<-'NA')
  return(temp_etfs)
}

url <- "https://etfdb.com/screener/#page=" 

etfs <- tibble()
remDr <- remoteDriver(browserName = "firefox",remoteServerAddr = "localhost", port = 4445L)
remDr$open()

#navigate to the url
main_page <- remDr$navigate(paste0(url,1))
Sys.sleep(1)
j=1
while (j <= 92) {
  #shell('docker pull selenium/standalone-firefox:2.53.0')
  #shell('docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0')
  curl<-paste0(url,j)
  main_page <- remDr$navigate(curl)
  Sys.sleep(3)
  remDr$refresh()
  main_page<-read_html(unlist(remDr$getPageSource()),encoding="UTF-8")
  data<-temp_etfs(main_page)
  etfs <- bind_rows(etfs, data)
  print(j)
  j<-j+1
}
etfs<-unique(etfs)
remDr$close()
write.csv(etfs,'etfs.csv')
print("script completed, please check the results in my documents")

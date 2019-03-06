require(R.utils)
library(openxlsx)
library(stringr)
#devtools::install_github("ropensci/RSelenium")
library(RSelenium)
#devtools::install_github("omegahat/Rcompression")


## setting up
WAIT = 5
SF = "E:temp"

ff64 = "c:/PROGRAMS/Firefox/FirefoxPortable/App/Firefox64/firefox.exe"
eCap1 <- list(`moz:firefoxOptions` = list(binary = ff64), pageLoadStrategy = 'none', timeouts = list(pageLoad = 10))
#timeouts = list(script = 5, pageLoad = 10))
eCap2 <- makeFirefoxProfile(list("browser.download.panel.shown" = FALSE,
                                 "browser.download.manager.showWhenStarting" =  FALSE,
                                 "browser.download.dir" = SF,
                                 "browser.download.folderList" = 2L,
                                 "browser.download.manager.closeWhenDone" = TRUE,
                                 "browser.download.manager.showAlertOnComplete" = FALSE,
                                 "browser.download.animateNotifications" = FALSE,
                                 "browser.helperApps.neverAsk.saveToDisk" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
))

rS = rsDriver(browser = "firefox", port = 4566L, extraCapabilities = c(eCap1, eCap2))
rDr <- rS[['client']]


#rDr$open()
#rDr$close()
#rS$server$stop()
#rDr$navigate("about:config")

# whole file
idf <- read.xlsx("Indices.xlsx")


### testing BNP:
bnp <- idf[which(str_detect(idf$Index, "^BNP")),]

# go disclamer page
rDr$navigate(url = "https://indx.bnpparibas.com/PreDisclaimer/Index")
Sys.sleep(WAIT)

# pass first disclamer
cb1 <- rDr$findElement(using = "xpath", "//label[@for='checkbox_1']")
cb3 <- rDr$findElement(using = "xpath", "//label[@for='checkbox_3']")
cb1$clickElement()
cb3$clickElement()
vb <-  rDr$findElement(using = "xpath", "//a[@ng-click='validatePreDisclaimer()']")
vb$clickElement()
Sys.sleep(WAIT)

# pass second disclamer
cb1 <- rDr$findElement(using = "xpath", "//label[@for='checkbox_1']")
cbr <- rDr$findElement(using = "xpath", "//label[@for='chkRem']")
cb1$clickElement()
cbr$clickElement()
bv <- rDr$findElement(using = "xpath", "//button[@id='btnValidate']")
bv$clickElement()
Sys.sleep(WAIT)

# get files
for (i in 1:nrow(bnp)) {  #i=i+1
  #download.file(bnp$Historical.data[i], destfile = paste0("indices/",bnp$Symbol[i],".xls"))
  rDr$navigate(bnp$Historical.data[i])
  Sys.sleep(WAIT)
  exp <- rDr$findElement(using = "xpath", "//a[@id='export-history-excel']")
  hrf = exp$getElementAttribute(attrName = 'href')
  
  # navigating stores files in browser default download place (eg. /user/Downloads)
  try(rDr$navigate(hrf[[1]]))
  Sys.sleep(WAIT)
  
  # renaming
  renameFile(paste0(SF,"/Index.xlsx"), paste0(SF,"/",bnp$Symbol[i],".xlsx"), overwrite = T)
  
  #download.file(hrf[[1]], destfile = "E:/temp/x.xls", extra = )
  #tryCatch(expr = evalWithTimeout(rDr$navigate(hrf[[1]]), cpu = 2, timeout = 3),
  #         TimeoutException = function(ex) cat("Timeout. Skipping.\n"))
  
  # pass to wget
  #cookies = rDr$getAllCookies()
  #url = hrf[[1]]
  #filename = paste0("E:/temp/",bnp$Symbol)
}

gc()



### testing ftse:
ftse <- idf[which(str_detect(idf$Historical.data, "ftse.com/")),]

# load main page of historic indexes
ftse_hix <- "https://www.ftse.com/analytics/factsheets/Home/HistoricIndexValues"
rDr$navigate(ftse_hix)
Sys.sleep(WAIT*2)

# iterate thru all rows
tr <- rDr$findElements(using = "xpath", "//tr[@class='Highlight historicValuesRow']")
i = 0
repeat {
  i = i + 1
  if (i > length(tr)) break 
  
  #download.file(ftse$Historical.data, ftse$Symbol, "wget", extra = "")
  idx <- tr[[i]]$findChildElement("xpath", "td[@data-title='Index']")
  idx <- idx$getElementText()[[1]]
  cur <- tr[[i]]$findChildElement("xpath", "td[@data-title='Currency']")
  cur <- cur$getElementText()[[1]]
  ttr <- tr[[i]]$findChildElement("xpath", "td[@data-title='Tax Treatment']")
  ttr <- ttr$getElementText()[[1]]
  
  nix <- try(tr[[i]]$findChildElement("xpath", "td[@data-title='Net Index']/a"))
  if (class(nix) != "try-error") {
    nixl <- nix$getElementAttribute('href')[[1]]
    #rDr$navigate(nixl)
    ox = loadWorkbook(nixl)
    saveWorkbook(ox, file = paste0("FTSE/NetIdx_",idx,"_",cur,".xlsx"))
  }
  
  cri <- try(tr[[i]]$findChildElement("xpath", "td[@data-title='Total Ret & Cap Ret Index']/a"))
  if (class(cri) != "try-error") {
    cril <- cri$getElementAttribute('href')[[1]]
    #rDr$navigate(cril)
    ox = loadWorkbook(cril)
    saveWorkbook(ox, file = paste0("FTSE/TotalRet_",idx,"_",cur,".xlsx"))
  }
  
  print(paste("i:",i,"|",idx,cur))
  Sys.sleep(WAIT/5)
}

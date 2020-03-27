library(rvest)

get_six_details <- function(ISIN, currency = "CHF", category = "funds", output = "df") {
  
  if (category == "funds") {
    category_short <- "FU"
  } else if (category == "shares") {
    category_short <- "EQ"
  } else {
    stop("category has to be funds or shares")
  }
  
  url<-paste0("https://www.six-group.com/exchanges/", category, "/info_details_en.html?id=", ISIN, currency, "4&portalSegment=", category_short)
  
  D1 <- read_html(url) %>% 
    html_nodes("tr") %>%
    html_nodes("td.last") %>%
    html_text()
  
  Valor_symbol<-D1[1]
  Valor_number<-D1[2]
  ISIN<-D1[3]
  Trading_currency<-D1[4]
  Exchange<-D1[6]
  Product_type<-D1[7]
  Trading<-D1[8]
  Fund_type<-D1[9]
  Smallest_tradeable_unit<-D1[10]
  Asset_class<-D1[12]
  Domicile_of_fund<-D1[13]
  Investment_region<-D1[14]
  Management_style<-D1[16]
  Market_expectation<-D1[17]
  Replication_method<-D1[18]
  Fund_manager<-D1[20]
  Dividend_entitlement<-D1[22] 
  
  D2 <- read_html(url) %>% 
    html_nodes("tr") %>%
    #html_nodes("td.last") %>%
    html_text()
  
  Underlying<-D2[grep('Underlying',D2)]
  Underlying<-Underlying[2]
  Underlying<-gsub('Underlying','',Underlying)
  
  Index_provider<-D2[grep('Index provider',D2)]
  Index_provider<-Index_provider[2]
  Index_provider<-gsub('Index provider','',Index_provider)
  
  Number_in_issue<-D2[grep('Number in issue',D2)]
  Number_in_issue<-Number_in_issue[2]
  Number_in_issue<-gsub('Number in issue','',Number_in_issue)
  
  Fund_currency<-D2[grep('Fund currency',D2)]
  Fund_currency<-Fund_currency[2]
  Fund_currency<-gsub('Fund currency','',Fund_currency)
  
  Management_fee<-D2[grep('Management fee',D2)]
  Management_fee<-Management_fee[2]
  Management_fee<-gsub('Management fee','',Management_fee)
  
  Bloomberg_symbol<-D2[grep('Bloomberg symbol',D2)]
  Bloomberg_symbol<-Bloomberg_symbol[2]
  Bloomberg_symbol<-gsub('Bloomberg symbol','',Bloomberg_symbol)
  
  Reuters_symbol<-D2[grep('Reuters symbol',D2)]
  Reuters_symbol<-Reuters_symbol[2]
  Reuters_symbol<-gsub('Reuters symbol','',Reuters_symbol)
  
  if (output == "df") {
    out<-data.frame(Valor_symbol,Valor_number,ISIN,Trading_currency,Exchange,Product_type,Trading,Fund_type,Smallest_tradeable_unit,
                    Asset_class,Domicile_of_fund,Investment_region,Management_style,Market_expectation,Replication_method,Fund_manager,
                    Dividend_entitlement,Underlying,Index_provider,Number_in_issue,Fund_currency,Management_fee,Bloomberg_symbol,
                    Reuters_symbol, 
                    stringsAsFactors = F)
  } else if (output == "list"){
    out<-list(Valor_symbol=Valor_symbol,Valor_number=Valor_number,ISIN=ISIN,Trading_currency=Trading_currency,Exchange=Exchange,
              Product_type=Product_type,Trading=Trading,Fund_type=Fund_type,Smallest_tradeable_unit=Smallest_tradeable_unit,
              Asset_class=Asset_class,Domicile_of_fund=Domicile_of_fund,Investment_region=Investment_region,Management_style=Management_style,
              Market_expectation=Market_expectation,Replication_method=Replication_method,Fund_manager=Fund_manager,
              Dividend_entitlement=Dividend_entitlement,Underlying=Underlying,Index_provider=Index_provider,Number_in_issue=Number_in_issue,
              Fund_currency=Fund_currency,Management_fee=Management_fee,Bloomberg_symbol=Bloomberg_symbol,Reuters_symbol=Reuters_symbol)
  } else {
    stop("output has to be df or list")
  }
  
  return(out)
}

get_six_dividends <- function(ISIN, currency = "CHF", category = "funds") {
  
  if (category == "funds") {
    category_short <- "FU"
  } else if (category == "shares") {
    category_short <- "EQ"
  } else {
    stop("category has to be funds or shares")
  }
  
  url<-paste0("https://www.six-group.com/exchanges/", category, "/info_details_en.html?id=", ISIN, currency,"4&portalSegment=", category_short)
  D1 <- read_html(url) %>% 
    html_nodes("table.table-grid") %>%
    html_nodes("td") %>%
    html_text()
  df <- data.frame(matrix(unlist(D1), ncol=3, byrow=T), stringsAsFactors = F)
  colnames(df) <-c("Ex_dividend_date","Value","Currency")
  df <- df[-c(1, 2),]
  return(df)
}


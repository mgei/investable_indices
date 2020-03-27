options(shiny.reactlog = TRUE)

library(shiny)
library(argonR)
library(argonDash)
# library(tidyverse)
library(tidyquant)
library(lubridate)
library(shinyWidgets)
# library(shinysky)
library(plotly)
library(httr)
library(readxl)

cache_dir <- "data/cache/"

get_prices <- function(symbol, 
                       from = (floor_date(Sys.Date() - period(10, units = "years"), "month") - 1), 
                       to = (floor_date(Sys.Date(), "month") - 1)) {
  if (!is.Date(from)) {
    # print("from isnt a date")
    from <- ymd(from)
  }
  if (!is.Date(to)) {
    # print("to isnt a date")
    to <- ymd(to)
  }
  if (is.na(from) | is.na(to)) {
    stop("dates to/from are invalid")
  }
                         
  if (paste0(symbol, ".RDS") %in% list.files(cache_dir)) {
    # print("exists in chache")
    cached <- readRDS(paste0(cache_dir, symbol, ".RDS"))
    
    if (cached$from <= from & cached$to >= to) {
      # print("only using cached data")
      out <- cached$data
      
      out <- out %>% 
        filter(date >= from,
               date <= to)
      
      return(out)
    }
  }
  
  # get from yahoo finance
  # print("get data from yh")
  out <- tq_get(x = symbol, from = from, to = to)
  
  if (!is_tibble(out)) {
    stop("no data available")
  }
  
  out <- out %>% 
    mutate("symbol" = symbol)
  
  if (exists("cached")) {
    # print("cached was loaded exists")
    out <- anti_join(cached$data, out, by = "date") %>% 
      bind_rows(out)
    
    from <- min(from, min(out$date))
    to <- max(to, max(out$date))
  }
  
  # save to cached files
  list(from = from,
       to = to,
       data = out) %>% 
    saveRDS(paste0(cache_dir, symbol, ".RDS"))
  
  return(out)
}


onRenderRebaseTxt <- "
    function(el, x) {
      el.on('plotly_relayout', function(rlyt) {
        var nrTrcs = el.data.length;
        // array of x index to rebase to; defaults to zero when all x are shown, needs to be one per trace
        baseX = Array.from({length: nrTrcs}, (v, i) => 0);
        // if x zoomed, increase baseX until first x point larger than x-range start
        if (el.layout.xaxis.autorange == false) {
            for (var trc = 0; trc < nrTrcs; trc++) {
                while (el.data[[trc]].x[baseX[trc]] < el.layout.xaxis.range[0]) {baseX[trc]++;}
            }   
        }
        // rebase each trace
        for (var trc = 0; trc < nrTrcs; trc++) {
            el.data[trc].y = el.data[[trc]].y.map(x => x / el.data[[trc]].y[baseX[trc]]);
        }
        el.layout.yaxis.autorange = true; // to show all traces if y was zoomed as well
        el.layout.datarevision++; // needs to change for react method to show data changes
        Plotly.react(el, el.data, el.layout);
      });
    }
    "


reload_fundlist <- function() {
  url <- "https://www.six-group.com/exchanges/funds/explorer_export_en.xls"
  GET(url, write_disk("data/fundlist.xls", overwrite = T))
  fundlist <- read_xls("data/temp.xls", skip = 4)
  
  fundlist %>% saveRDS("data/fundlist.RDS")
  
  return(fundlist)
}

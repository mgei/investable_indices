library(tidyverse)
library(tidyquant)
library(magrittr)
library(plotly)
library(htmlwidgets)

# load data we have in csv form ----
directory <- list.files("pricedata/.")

# empty tibble for prices, needs the date column
pricedata <- tibble(date = as.Date("1900-01-01")) %>% 
  filter(date != as.Date("1900-01-01"))

for (file in directory) {
  print(paste(which(directory == file), file))
  # make it monthly
  temp <- read_csv(paste0("pricedata/", file), col_types = "Ddddddd") %>% 
    tq_transmute(select     = open:adjusted, 
                 mutate_fun = to.period, 
                 period     = "weeks") %>% 
    mutate(date = ceiling_date(date, unit = "weeks") - days(1)) %>% 
    select(date, !!str_remove(file, ".csv") := adjusted)
  
  pricedata %<>% full_join(temp, by = "date")
  rm(temp)
  
  print("done")
}

pricedata %>% 
  arrange(date) %>%
  gather(symbol, price, -date) %>% 
  filter(!is.na(price), !is.na(symbol)) %>% 
  arrange(symbol, date) %>%  
  group_by(symbol) %>% 
  fill(price, .direction = "down") %>% 
  tq_transmute(select     = price, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               type       = "arithmetic") -> wrets

wrets %>% 
  mutate(launch = min(date), launchdbl = as.double(launch), launchsymbol = str_c(launchdbl, symbol)) %>% 
  arrange(launchsymbol) -> temp

temp %>% 
  select(symbol) %>% 
  distinct() %>% 
  pull() -> symbol_colnames

temp %>% 
  ungroup() %>%
  select(-launch, -launchdbl, -symbol) %>% 
  spread(launchsymbol, weekly.returns) %>% 
  fill(2:ncol(.), .direction = "down") -> temp

colnames(temp) <- c("date", symbol_colnames)

temp %>% 
  filter(row_number() != 1) -> wrets_wide

wrets_wide %>% 
  mutate(available = rowSums(!is.na(.))-1) %>% 
  select(available, everything()) %>% 
  filter(available >= 500) %>% 
  select(2:(.$available[1]+2)) -> yolo

yolo %>% View()

yolo %>% 
  select(-date) %>% 
  as.matrix() %>% 
  prcomp(scale. = T, center = T)


## simulate ----

simulate_performance <- function(returns, xdate, wback, wforth) {
  
  if (class(xdate) != "Date") {
    xdate <- as.Date(xdate)
  }
  
  xdate <- returns %>% 
    ungroup() %>% 
    select(date) %>% 
    distinct() %>% 
    arrange(date) %>% 
    mutate(dat = abs(date - xdate)) %>%
    filter(dat == abs(min(dat))) %>% 
    select(date) %>% 
    first() %>% 
    pull()
  
  xminus <- xdate - weeks(wback)
  xplus  <- xdate + weeks(wforth)  
  
  training <- wrets %>% 
    group_by(symbol) %>% 
    filter(first(date) <= xminus, last(date) >= xplus) %>% 
    ungroup() %>% 
    filter(date >= xminus, date <= xdate) %>% 
    # the following is to avoid the symbols that have a gap in the return history (we need returna for all weeks)
    group_by(symbol) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(n == max(n)) %>% 
    select(-n)
  
  training_matrix <- training %>% 
    spread(symbol, weekly.returns, fill = 0) %>% 
    select(-date) %>% 
    as.data.frame() %>% 
    as.matrix()
  
  # training_matrix %>% View()
  
  training_matrix %>% prcomp(scale. = T, center = T) %>% .$rotation %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "symbol") %>% 
    as_tibble() -> pca_loadings
  
  top_loadings <- pca_loadings %>%
    # mutate_if(is.numeric, abs) %>% 
    select(1:5) %>% 
    filter_if(is.numeric, any_vars(. %in% c(min(.),max(.)))) %>% 
    mutate_if(is.numeric, function(x){if_else(x %in% c(min(x), max(x)), paste0(as.character(round(x, 3)), "*"), as.character(round(x, 3)))}) %>% 
    left_join(etfs %>% select(symbol = etf.symbol.text, name = etf.name.text, assetclass = etf.asset_class),
              by = "symbol") %>% 
    select(symbol, name, assetclass, PC1, PC2, PC3, PC4)
  
  top_loadings
  
  
  
  pca_symbols <- top_loadings %>% 
    pull(symbol)
  
  pca_symbols_pricedata <- pricedata %>%
    gather(symbol, adjusted, -date) %>% 
    filter(symbol %in% pca_symbols) %>% 
    group_by(symbol) %>% 
    arrange(date) %>% 
    fill(adjusted) %>% 
    ungroup() %>% 
    filter(!is.na(adjusted))
    
  
  performance_plot(pca_symbols_pricedata, from = xminus, to = xdate)
  
  performance_plot(pca_symbols_pricedata)
  
  
  
  
  pca_loadings %>%   
    mutate_if(is.numeric, abs) %>% 
    filter(symbol %in% c("SPY", "GLD", "AGG", "IVV"))
  
  pca_loadings %>% 
    mutate_if(is.numeric, abs) %>%
    arrange(desc(PC1))
  
  training_matrix[,c(1,2,223,224,225)] %>% View()
  
  # FVC 
  wrets %>% 
    ungroup() %>% 
    filter(symbol == "FVC") %>% View()
    ggplot(aes(x = date, y = weekly.returns)) +
    geom_col()
    
    group_by(symbol) %>% 
    summarise(f = first(date),
              l = last(date))
    group_by(symbol) %>% 
  }
  
  
  
}

returns <- wrets

xdate <- as.Date("2016-01-02")



performance_plot <- function(pdata, from = NA, to = NA) {
  
  if (is.na(from)) {
    from <- min(pdata$date)
  }
  if (is.na(to)) {
    from <- max(pdata$date)
  }
  
  
  pdata %>% 
    group_by(symbol) %>% 
    summarise(adj1 = adjusted[1L])
  
  
  pltly <- 
    pdata %>% 
    dplyr::group_by(symbol) %>% 
    dplyr::mutate(adjusted = adjusted / adjusted[1L]) %>% 
    plotly::plot_ly(x = ~date, y = ~adjusted, color = ~symbol,
                    type = "scatter", mode = "lines") %>% 
    plotly::layout(dragmode = "zoom", 
                   datarevision = 0) %>% 
    rangeslider(start = from, end = to)
  
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
  
  htmlwidgets::onRender(pltly, onRenderRebaseTxt)
}


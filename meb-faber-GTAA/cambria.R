# Cambria Shareholder Yield ETF (SYLD)
# Cambria Foreign Shareholder Yield ETF (FYLD)
# Cambria Global Value ETF (GVAL)
# Cambria Global Momentum ETF (GMOM)
# Cambria Global Asset Allocation ETF (GAA)
# Cambria Emerging Shareholder Yield ETF (EYLD)
# Cambria Value and Momentum ETF (VAMO)
# Cambria Sovereign Bond ETF (SOVB)
# Cambria Tail Risk ETF (TAIL)
# Cambria Trinity ETF (TRTY)
# Cambria Cannabis ETF (TOKE)

etf_symbols <- c("SYLD",
                 "FYLD",
                 "GVAL",
                 "GMOM",
                 "GAA",
                 "EYLD",
                 "VAMO",
                 "SOVB",
                 "TAIL",
                 "TRTY",
                 "TOKE",
                 "SPY")

prices <- tq_get(etf_symbols, from = "2000-01-01")

prices %>% 
  group_by(symbol) %>% 
  summarise(since = min(date)) %>% 
  arrange(since)

library(dygraphs)
library(tidyquant)
library(timetk)
library(tidyverse)

library(tidyquant)
library(plotly)
library(htmlwidgets)
library(dplyr)


pltly <- prices %>% 
  filter(date > as.Date("2013-01-01")) %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::mutate(adjusted = adjusted / adjusted[1L]) %>% 
  plotly::plot_ly(x = ~date, y = ~adjusted, color = ~symbol,
                  type = "scatter", mode = "lines") %>% 
  plotly::layout(dragmode = "zoom", 
                 datarevision = 0) %>% 
  plotly::rangeslider()

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

prices %>% 
  filter(date > as.Date("2013-01-01")) %>% 
  group_by(symbol) %>% 
  mutate(Ra = adjusted/lag(adjusted) -1) %>% 
  group_by(symbol, year = year(date)) %>% 
  summarise(Ra_yr = (prod(1+Ra, na.rm = T) - 1),
            Vola_yr = sd(Ra, na.rm = T)*sqrt(250)) %>% 
  ungroup() %>% 
  mutate(SR = Ra_yr/Vola_yr) %>% 
  filter(!is.na(SR)) %>% 
  ggplot(aes(x = year, y = SR, color = symbol)) +
  geom_point(aes(size = symbol == "SPY"))

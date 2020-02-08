library(tidyverse)
library(scales)
library(plotly)
library(htmlwidgets)
library(lubridate)

# Migrosbank Funds

funds <- tribble(
  ~Name, ~NAV, ~R1Y, ~R3Y, ~Valor, ~ISIN,
  "Migros Bank (CH) Fonds 0 V",	"CHF	103.60", "+3.22%",	"n.v.", 36569669, "CH0365696696",
  "Migros Bank (CH) Fonds 25 V", "CHF	124.69", "+6.39%",	"+10.88%", 2340649, "CH0023406496",
  "Migros Bank (CH) Fonds 45 V", "CHF	113.09", "+9.33%",	"n.v.", 2340656, "CH0023406561",
  "Migros Bank (CH) Fonds Sustainable 0 V", "CHF	103.02", "+2.84%", "n.v.", 36569685, "CH0365696852",
  "Migros Bank (CH) Fonds Sustainable 25 V", "CHF	108.83", "+6.65%", "n.v.", 36569690, "CH0365696902",
  "Migros Bank (CH) Fonds Sustainable 45 V", "CHF	149.33", "+10.45%", "+17.35", 10270610, "CH0102706105"
)

# https://www.swissfunddata.ch/sfdpub/anlagefonds

pricedata <- tibble()
for (fund in 1:nrow(funds)) {
  val <- pull(funds[fund, "Valor"])
  
  temp <- read_delim(paste0("returns/", val, ".csv"), delim = ";", skip = 2, locale=locale(decimal_mark = ",")) %>% 
    mutate(Valor = val)
  
  pricedata <- bind_rows(pricedata, temp)
}

nav <- pricedata %>% 
  select(-"Issue Price", -"Redemption Price", -"Closing Price", -starts_with("CCY")) %>% 
  pivot_longer(-c(Valor, Date)) %>%
  # filter(Valor == val) %>% 
  filter(name %in% c("Net Asset Value")) %>% 
  left_join(funds, by = "Valor")
  
  
nav %>% 
  ggplot(aes(x = Date, y = value, color = factor(Valor))) +
  geom_line(aes(linetype = name)) +
  scale_y_continuous(labels = comma)

pltly <- nav %>% 
  group_by(Name) %>% 
  mutate(value = value / value[1L]) %>% 
  plot_ly(x = ~Date, y = ~value, color = ~Name,
          type = "scatter", mode = "lines") %>% 
  layout(dragmode = "zoom", 
         datarevision = 0) %>% 
  rangeslider()

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


nav %>% 
  group_by(Name, year = year(Date)) %>% 
  tq_mutate(select = value, mutate_fun = periodReturn,
            period = "daily", type = "log", col_rename = "R") %>% 
  select(Date, Name, R) %>% 
  tq_performance(Ra = R, performance_fun = table.AnnualizedReturns) %>% 
  ungroup() %>% 
  arrange(year, Name) %>% 
  clean_names() %>% 
  mutate(category = str_sub(name, -4) %>% str_trim(),
         type = if_else(str_detect(name, "Sustainable"), "Sustainable", "Ordinary")) %>% 
  # filter(year >= 2017) %>% 
  filter(year %in% 2009:2019) %>% 
  ggplot(aes(x = annualized_std_dev, y = annualized_return)) +
  geom_point(aes(color = name, shape = type), size = 5, alpha = 0.8) +
  geom_path(aes(group = interaction(year, category)), alpha = 0.5, color = "red") +
  geom_text(aes(label = ifelse(type == "Ordinary", year, "")), nudge_x = 0.004, size = 3) +
  geom_path(aes(group = year, linetype = factor(year)), alpha = 0.5) +
  scale_color_discrete("", labels = unique(nav$Name)) +
  scale_shape_discrete("") +
  scale_linetype(guide = F) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.4) +
  labs(title = "Migros Bank Vorsorgefonds",
       subtitle = "annual returns and volatility",
       x = "sigma", y = "mu")
  # geom_smooth(aes(color = name), method = "lm", fullrange = T, se = F)


tq_performance_fun_options()  

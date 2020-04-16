source("../shiny-comparison/setup.R")

fundlist_SR <- readRDS("fundlist_SR.RDS")
fundlist_stats_long <- readRDS("fundlist_stats_long.RDS")
index_SR <- readRDS("index_SR.RDS")

current_year <- year(Sys.Date())

green_red_colors <- colorRamps::green2red(256) %>% rev()
color_breaks <- function(x_range, midpoint) {
  if (is.na(range(x_range) %>% diff())) {
    return(1:255)
  }
  
  largerhalfrange <- max(abs(midpoint-min(x_range, na.rm = T)), 
                         abs(midpoint-max(x_range, na.rm = T)))
  
  # halfrange <- max(abs(x_range))-abs(midpoint)
  breaks <- seq(midpoint-largerhalfrange, 
                midpoint+largerhalfrange, 
                length.out = 255)
  # breaks <- seq(midpoint-(max(abs(x_range))-abs(midpoint)), 
  #               midpoint+(max(abs(x_range))-abs(midpoint)), 
  #               length.out = 255)
  return(breaks)
}

ui <- fluidPage(
  fluidRow(
    column(2,
           pickerInput(
             inputId = "management_style",
             label = "Management style", 
             choices = c("active", "passive"))
    ),
    column(2,
           pickerInput(
             inputId = "asset_class",
             label = "Asset class", 
             choices = c("Commodities", "Equity Developed Markets",
                         "Equity Emerging Markets", "Equity Strategy",
                         "Equity Themes", "Fixed Income", "Other",
                         "Real Estate", "Funds", "Money Market",
                         "Volatility"))
    ),
    column(2,
           pickerInput(
             inputId = "stat_field",
             label = "Stat field for comparison", 
             choices = c("Return", "TotalReturn",
                         "Volatility", "SharpeRatio"),
             selected = "SharpeRatio")
    ),
    column(2,
           pickerInput(
             inputId = "year_since",
             label = "Year since", 
             choices = 1998:current_year,
             selected = 2015)
    )
  ),
  fluidRow(
    column(12,
           # DTOutput("index_table"),
           DTOutput("key_table"))
  )
)

server <- function(input, output, session) {
  
  output$key_table <- renderDT({
    req(input$management_style)
    req(input$asset_class)
    req(input$stat_field)
    req(input$year_since)
    
    year_range <- seq(input$year_since, current_year, 1) %>% 
      paste(collapse = "|")
    
    d <- fundlist_stats_long %>%
      filter(`Management style` == input$management_style, `Asset class` == input$asset_class,
             field == input$stat_field,
             str_detect(year, year_range)) %>%
      # filter(field == "SharpeRatio") %>%
      select(Name, `Trading currency`, ISIN, `Management fee`, year, value) %>%
      pivot_wider(names_from = "year",
                  values_from = "value") %>%
      select(Name, `Trading currency`, ISIN, `Management fee`, colnames(.)[which(colnames(.) %>% str_detect("^19|^20|^since"))] %>% sort())

    datatable(d)
    
  })
  
  # output$index_table <- renderDT({
  #   d <- index_SR %>%
  #     select(Name = Index, as.character(2010:2020))
  #   
  #   datatable(d, rownames = F,
  #             options = list(autoWidth = T,
  #                            columnDefs = list(list(width = '60px', targets = 2:11)))) %>% 
  #     formatRound(c(2:12), 2)
  # })

#   output$key_table <- renderDT({
#     req(input$management_style)
#     req(input$asset_class)
#     
#     d <- fundlist_SR %>%
#       filter(`Management style` == input$management_style, `Asset class` == input$asset_class) %>%
#       select(Name, as.character(2010:2020)) %>%
#       rowwise() %>%
#       mutate(m = mean(c(`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`), na.rm = T)) %>%
#       ungroup() %>% 
#       arrange(desc(m)) %>%
#       select(-m)
#     
#     datatable(d, rownames = F,
#               options = list(pageLength = 20, lengthChange = F,
#                              autoWidth = T,
#                              columnDefs = list(list(width = '60px', targets = 2:11)))) %>% 
#       formatRound(c(2:12), 2) %>% 
#       formatStyle("2010", backgroundColor = styleInterval(color_breaks(d$`2010`, index_SR$`2010`[1]), green_red_colors)) %>%
#       formatStyle("2011", backgroundColor = styleInterval(color_breaks(d$`2011`, index_SR$`2011`[1]), green_red_colors)) %>%
#       formatStyle("2012", backgroundColor = styleInterval(color_breaks(d$`2012`, index_SR$`2012`[1]), green_red_colors)) %>%
#       formatStyle("2013", backgroundColor = styleInterval(color_breaks(d$`2013`, index_SR$`2013`[1]), green_red_colors)) %>%
#       formatStyle("2014", backgroundColor = styleInterval(color_breaks(d$`2014`, index_SR$`2014`[1]), green_red_colors)) %>% 
#       formatStyle("2015", backgroundColor = styleInterval(color_breaks(d$`2015`, index_SR$`2015`[1]), green_red_colors)) %>% 
#       formatStyle("2016", backgroundColor = styleInterval(color_breaks(d$`2016`, index_SR$`2016`[1]), green_red_colors)) %>% 
#       formatStyle("2017", backgroundColor = styleInterval(color_breaks(d$`2017`, index_SR$`2017`[1]), green_red_colors)) %>% 
#       formatStyle("2018", backgroundColor = styleInterval(color_breaks(d$`2018`, index_SR$`2018`[1]), green_red_colors)) %>% 
#       formatStyle("2019", backgroundColor = styleInterval(color_breaks(d$`2019`, index_SR$`2019`[1]), green_red_colors)) %>% 
#       formatStyle("2020", backgroundColor = styleInterval(color_breaks(d$`2020`, index_SR$`2020`[1]), green_red_colors))
#   })
}

#https://rstudio.github.io/DT/010-style.html

shinyApp(ui, server)
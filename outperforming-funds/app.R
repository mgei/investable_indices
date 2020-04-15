source("../shiny-comparison/setup.R")

fundlist_SR <- readRDS("fundlist_SR.RDS")
index_SR <- readRDS("index_SR.RDS")

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
    )
  ),
  fluidRow(
    column(12,
           DTOutput("key_table"))
  )
)

server <- function(input, output, session) {

  output$key_table <- renderDT({
    req(input$management_style)
    req(input$asset_class)
    
    d <- index_SR %>%
      select(Name = Index, as.character(2010:2020)) %>%
      bind_rows(fundlist_SR %>%
                  filter(`Management style` == input$management_style, `Asset class` == input$asset_class) %>%
                  select(Name, as.character(2010:2020)) %>%
                  rowwise() %>%
                  mutate(m = mean(c(`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`), na.rm = T)) %>%
                  arrange(desc(m)) %>%
                  select(-m))
    
    datatable(d)
  })
}

#https://rstudio.github.io/DT/010-style.html

shinyApp(ui, server)
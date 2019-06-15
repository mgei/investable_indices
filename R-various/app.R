library(shiny)
library(plotly)
library(tidyquant)

stocks <- readRDS("data/stocks.RDS")
range_from <- as.Date("2019-02-01")

ui <- fluidPage(
    titlePanel("Rangesliding performance"),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("plot")
        )
)

server <- function(input, output) {
  
  stocks_range <- stocks %>% 
    filter(date >= range_from) %>% 
    group_by(symbol) %>% 
    mutate(performance = adjusted/first(adjusted)-1)

    output$plot <- renderPlotly({
      
      p <- stocks_range %>% 
        ggplot(aes(x = date, y = performance, color = symbol)) +
        geom_line()
      
      ggplotly(p, dynamicTicks = T) %>%
        rangeslider(borderwidth = 1) %>%
        layout(hovermode = "x", yaxis = list(tickformat = "%"))
    
      })
}

shinyApp(ui = ui, server = server)

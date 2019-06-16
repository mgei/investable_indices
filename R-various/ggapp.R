# trying the same with ggplot and ggplotly but unfortunately this does not work

library(shiny)
library(plotly)
library(tidyquant)
library(lubridate)

stocks <- readRDS("data/stocks.RDS")
range_from <- as.Date("2019-01-01")

ui <- fluidPage(
  titlePanel("Rangesliding performance"),
  
  mainPanel(
    plotlyOutput("plot")
  )
)

server <- function(input, output) {
  
  d <- reactive({ e <- event_data("plotly_relayout")
  if (is.null(e)) {
    e$xaxis.range <- c(range_from, max(stocks$date))
  }
  e })
  
  stocks_range_dyn <- reactive({
    s <- stocks %>%
      filter(date >= range_from) %>%
      group_by(symbol) %>%
      mutate(performance = adjusted/first(adjusted)-1)
    
    if (!is.null(d())) {
      s <- s %>%
        mutate(performance = adjusted/nth(adjusted, which.min(abs(date - date(d()$xaxis.range[[1]]))))-1)
    }
    
    s
    
  })
  
  output$plot <- renderPlotly({
    
    # plot_ly(stocks_range_dyn(), x = ~date, y = ~performance, color = ~symbol) %>% 
    #   add_lines() %>%
    #   rangeslider(start =  d()$xaxis.range[[1]], end =  d()$xaxis.range[[2]])
    
    p <- stocks_range_dyn() %>% 
      ggplot(aes(x = date, y = performance, color = symbol)) +
      geom_line()
    
    ggplotly(p, dynamicTicks = T) %>%
      rangeslider(start =  d()$xaxis.range[[1]], end =  d()$xaxis.range[[2]]) %>%
      layout(hovermode = "x", yaxis = list(tickformat = "%"))
    
  })
  
}

shinyApp(ui = ui, server = server)
library(shiny)
library(plotly)
library(tidyquant)
library(lubridate)

stocks <- tq_get(c("AAPL", "MSFT"), from = "2019-01-01")
# stocks <- readRDS("data/stocks.RDS")

ui <- fluidPage(
    titlePanel("Rangesliding performance"),
        mainPanel(
           plotlyOutput("plot")
        )
)

server <- function(input, output) {

  d <- reactive({ e <- event_data("plotly_relayout")
                  if (is.null(e)) {
                    e$xaxis.range <- c(min(stocks$date), max(stocks$date))
                  }
                  e })
  
  stocks_range_dyn <- reactive({
    s <- stocks %>%
      group_by(symbol) %>%
      mutate(performance = adjusted/first(adjusted)-1)
    
    if (!is.null(d())) {
      s <- s %>%
        mutate(performance = adjusted/nth(adjusted, which.min(abs(date - date(d()$xaxis.range[[1]]))))-1)
    }
    
    s
  })

    output$plot <- renderPlotly({

      plot_ly(stocks_range_dyn(), x = ~date, y = ~performance, color = ~symbol) %>% 
        add_lines() %>%
        rangeslider(start =  d()$xaxis.range[[1]], end =  d()$xaxis.range[[2]], borderwidth = 1)
      
      })
}

shinyApp(ui = ui, server = server)
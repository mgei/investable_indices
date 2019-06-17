library(shiny)
library(plotly)
library(tidyquant)
library(lubridate)

# stocks <- tq_get(c("AAPL", "MSFT"), from = "1990-01-01")
# stocks <- readRDS("data/stocks.RDS")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Dancing+Script');
      
      h2 {
        font-family: 'Dancing Script', cursive;
        font-weight: 500;
        line-height: 5;
        color: #000000;
      }
    "))
  ),
  
    titlePanel("Rangesliding performance"),
        mainPanel(
          textInput("tickers", "Tickers (use comma to separate)"),
          dateInput("fromdate", "from", value = "2000-01-01", min = "1960-01-01", max = Sys.Date()),
          actionButton("gobutton", "Go"),
          
          plotlyOutput("plot")
        )
)

server <- function(input, output) {
  
  stocksy <- eventReactive(input$gobutton, {
                                              t <- input$tickers %>% str_remove_all(" ") %>% str_split(",")
                                              tq_get(t[[1]], from = input$fromdate) #, from = "1990-01-01")
    })
  
  # stocksy <- eventReactive(input$gobutton, {
  #                                             stocks
  #   })
  
  # observe({ print(stocksy()) })

  d <- reactive({ e <- event_data("plotly_relayout")
                  if (is.null(e)) {
                    e$xaxis.range <- c(min(stocksy()$date), max(stocksy()$date))
                  }
                  e })
  
  stocks_range_dyn <- reactive({
    s <- stocksy() %>%
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
        rangeslider(start =  d()$xaxis.range[[1]], end =  d()$xaxis.range[[2]], borderwidth = 1) %>% 
        layout(xaxis = list(title = ""), 
               yaxis = list(title = "return performance", tickformat = "%",
                            autorange = T, fixedrange = F)) %>% 
        config(displayModeBar = F)
      
      })
}

shinyApp(ui = ui, server = server)
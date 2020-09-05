library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)
library(scales)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page


ui <- fluidPage(
  useShinyjs(), # Include shinyjs in the UI
  extendShinyjs(text = jsResetCode),
  
  fluidRow(
    column(2,
           uiOutput("last_reloaded"),
           actionBttn("reload_data", "Reload data",
              style = "fill", color = "danger")),
    column(10,
           h3("Rule: we are invested if and only if the end-of-month close price is larger than the end-of-month close price 2 months before.", align = "center"),
           plotOutput("plot1"),
           plotOutput("plot2"),
           plotOutput("plot3"))
  )
)

server <- function(input, output, session) {
  prices <- readRDS(paste0("data/",
                           list.files("data") %>% sort() %>% tail(1)))
  
  prices_monthly <- prices %>% 
    group_by(floor_date(Date, "months")) %>% 
    filter(Date == max(Date)) %>% 
    ungroup() %>% 
    mutate(invested = `Net Asset Value` > lag(`Net Asset Value`, n = 1L),
           invested = replace_na(invested, F),
           r = `Net Asset Value`/lag(`Net Asset Value`)-1,
           r = replace_na(r, 0),
           r_invested = r*lag(invested))
    
  output$last_reloaded <- renderUI({
    date <- list.files("data") %>% sort() %>% tail(1) %>% 
      str_remove(".RDS") %>% 
      as.Date()
    
    h5(paste0("Last reloaded on ", as.character(date)))
  })
  
  observeEvent(input$reload_data, {
    source("reload-data.R")
    
    js$reset()
  })
  
  output$plot1 <- renderPlot({
    prices %>% 
      ggplot(aes(x = Date, y = `Net Asset Value`)) +
      geom_line(alpha = 0.3) +
      geom_point(data = prices_monthly, aes(color = invested)) +
      geom_line(data = prices_monthly) +
      theme_bw()
  })
  
  output$plot2 <- renderPlot({
    prices %>% 
      mutate(Change = cumprod(1+r)-1) %>% 
      ggplot(aes(x = Date, y = Change)) +
      geom_line(alpha = 0.3) +
      geom_point(data = prices_monthly %>% mutate(Change = cumprod(1+r)-1), 
                 aes(color = invested)) +
      geom_line(data = prices_monthly %>% mutate(Change = cumprod(1+r)-1)) +
      geom_line(data = prices_monthly %>% mutate(Change = cumprod(1+r_invested)-1), color = "red") +
      scale_y_continuous(labels = percent) +
      scale_x_date(date_breaks = "1 year") +
      theme_bw()
  })
  
  output$plot3 <- renderPlot({
    prices %>% 
      group_by(floor_date(Date, "years")) %>% 
      mutate(Change = cumprod(1+r)-1) %>% 
      ungroup() %>% 
      mutate(Jahr = year(Date)) %>% 
      ggplot(aes(x = Date, y = Change)) +
      geom_line(alpha = 0.3) +
      geom_point(data = prices_monthly %>% group_by(floor_date(Date, "years")) %>% mutate(Change = cumprod(1+r)-1) %>% ungroup() %>% mutate(Jahr = year(Date)), 
                 aes(color = invested)) +
      geom_line(data = prices_monthly %>% group_by(floor_date(Date, "years")) %>% mutate(Change = cumprod(1+r)-1) %>% ungroup() %>% mutate(Jahr = year(Date))) +
      geom_line(data = prices_monthly %>% group_by(floor_date(Date, "years")) %>% mutate(Change = cumprod(1+r_invested)-1) %>% ungroup() %>% mutate(Jahr = year(Date)), 
                color = "red") +
      scale_y_continuous(labels = percent) +
      theme_bw() +
      labs(x = "") +
      facet_wrap(~Jahr, scales = "free_x", ncol = 6)
  })
}

shinyApp(ui, server)
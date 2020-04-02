source("setup.R")
source("data.R")


ui <- fluidPage(
  theme = "style.css",
  useShinydashboard(),
  fluidRow(
    column(1,
           actionButton(inputId = "reload_fund_list", label = "reload funds list"),
           checkboxInput(inputId = "activate_search_datatable", label = "search/filter funds", value = F)
           ),
    column(9,
           dataTableOutput("funds_list", width = "100%")
           ),
    column(2, "selected"
           )
    ),
  fluidRow(
    column(2,
           materialSwitch(
             inputId = "performance_price",
             label = "%-Performance",
             value = TRUE,
             right = T,
             status = "danger"),
           materialSwitch(
             inputId = "currency_chf",
             label = "im CHF",
             value = TRUE, 
             right = T,
             status = "danger")
           ),
    column(6,
           tabsetPanel(type = "tabs",
                       tabPanel(year(Sys.Date()), plotOutput("ytd_plot")),
                       tabPanel("2 years", plotOutput("2yr_plot")),
                       tabPanel("since inception", tableOutput("max_plot")))
           ),
    column(4, tabsetPanel(type = "tabs",
                          tabPanel("Holdings", plotOutput("holdingsplot")),
                          tabPanel("Summary", "summary"))),
    column(4, 
           "stats selected")
    ),
  fluidRow(
    column(12,
           "table")
  )
)

server <- function(input, output, session) {
  output$funds_list <- renderDataTable({
    fundlist %>% 
      datatable(filter = ifelse(input$activate_search_datatable, "top", "none"), 
                options = list(pageLength = 10, autoWidth = F, searching = T,  
                               columnDefs = list(list(targets = c(0, which(!(names(.) %in% c("Name", "ISIN", "Issuer", "Trading currency", "Management fee", "Investment region")))), 
                                                      visible = F)),
                               buttons = c('colvis'), dom = 'Bfritp',
                               # compact = T,
                               search = list(search = 'CHF')), 
                selection = "single", class = 'compact cell-border'#,  # extensions = 'Buttons', style = 'bootstrap4'     cell-border stripe
                # plugins = c("ellipses")
                ) %>%
      formatString(suffix = "%", columns = "Management fee") %>% 
      formatStyle(columns = 1:37, fontSize = '80%')
  }, server = T)
  
  data_selected <- reactive({
    req(input$funds_list_rows_selected)
    
    prices <- get_prices_cache(paste0(fundlist[input$funds_list_rows_selected, "Symbol"] %>% pull(), ".SW"))
    
    dividends <- get_six_dividends_cache(ISIN = fundlist[input$funds_list_rows_selected, "ISIN"] %>% pull(), 
                                         currency = fundlist[input$funds_list_rows_selected, "Trading currency"] %>% pull()) 
    
    details <- get_six_details_cache(ISIN = fundlist[input$funds_list_rows_selected, "ISIN"] %>% pull(), 
                                     currency = fundlist[input$funds_list_rows_selected, "Trading currency"] %>% pull())
    
    # don't get holdings from Yahoo if already there's no prices
    if (is.na(prices)) {
      holdings <- NA
    } else {
      holdings <- get_holdings_cache(symbol = fundlist[input$funds_list_rows_selected, "Symbol"] %>% pull())
    }
    
    out <- list(ISIN = fundlist[input$funds_list_rows_selected, "ISIN"],
                Symbol = fundlist[input$funds_list_rows_selected, "Symbol"],
                TradingCurrency = fundlist[input$funds_list_rows_selected, "Trading currency"],
                prices = prices,
                details = details,
                dividends = dividends,
                holdings = holdings)
    
    return(out)
  })
  
  output$ytd_plot <- renderPlot({
    
    req(data_selected())
    
    prices <- data_selected()$prices
    
    if (is_tibble(prices)) {
      
      if (input$performance_price) {
        p <- prices %>% 
          filter(date >= (Sys.Date() - years(2))) %>% 
          dplyr::group_by(symbol) %>% 
          dplyr::mutate(performance = (close / close[1L] - 1)) %>% 
          ungroup() %>% 
          filter(!is.na(performance)) %>% 
          ggplot(aes(x = date, y = performance)) +
          geom_line(aes(color = symbol)) +
          scale_y_continuous(labels = percent) +
          scale_x_date(date_labels = "%d.%m.%Y") +
          labs(title = "", x = "") +
          theme_bw()
        
        print(p)
      } else {
        p <- prices %>% 
          filter(date >= (Sys.Date() - years(2))) %>% 
          ggplot(aes(x = date, y = close)) +
          geom_line(aes(color = symbol)) +
          scale_y_continuous(labels = number) +
          scale_x_date(date_labels = "%d.%m.%Y") +
          labs(title = "", x = "", y = paste("closing price", data_selected()$TradingCurrency)) +
          theme_bw()
      }
        
      
      dividends <- data_selected()$dividends %>% 
        filter(Ex_dividend_date >= min(prices$date),
               Ex_dividend_date <= max(prices$date))
      
      if (nrow(dividends) > 0) {
        p <- p + 
          geom_label(data = data_selected()$dividends,
                     aes(y = 0, x = Ex_dividend_date, label = paste0(Currency, Value)),
                     alpha = 0.4)
      }
      p
    } else {
      plot_exception("no data is found")
    }
    
  })
  
  output$holdingsplot <- renderPlot({
    req(data_selected())
    
    holdings <- data_selected()$holdings
    
    if (is_tibble(holdings)) {
      if (nrow(holdings) > 0) {
        
        d <- holdings %>% 
          bind_rows(tibble(Company = "_other", holding_num = 1 - sum(.$holding_num)) %>% 
                      mutate(holding_num = max(holding_num, 0), holding = percent(holding_num, accuracy = 0.01))) %>% 
          ggplot(aes(x = "", y = holding_num, fill = Company)) +
          geom_col() +
          geom_text_repel(aes(label = holding), position = position_stack(vjust = .5), size = 4) +
          coord_polar("y", start = 0) +
          labs(title = "Fund holdings",
               fill = "") +
          theme_void() +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 9))
        
        d
        
      } else {
        plot_exception("no holdings data is found")
      }
    } else {
      plot_exception("no holdings data is found")
    }
  })
  
}

shinyApp(ui, server)
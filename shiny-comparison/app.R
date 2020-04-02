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
    column(12,
           uiOutput("missing_data"))
    ),
  fluidRow(
    column(2,
           materialSwitch(
             inputId = "currency_chf",
             label = "im CHF",
             value = TRUE, 
             status = "danger")
           ),
    column(6,
           tabsetPanel(type = "tabs",
                       tabPanel(year(Sys.Date()), plotOutput("ytd_plot")),
                       tabPanel("2 years", plotOutput("2yr_plot")),
                       tabPanel("since inception", tableOutput("max_plot")))
    ),
    column(4, tabsetPanel(type = "tabs",
                          tabPanel("Holdings", plotlyOutput("holdingsplot")),
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
  
  output$missing_data <- renderUI({
    
    req(data_selected())
    
    if (is.na(data_selected()$prices)) {
      shinydashboard::box(title = "Missing data", background = "yellow",
                          width = 12)
    }
    
  })
  
  
  output$ytd_plot <- renderPlot({
    
    req(data_selected())
    
    if (!is.na(data_selected()$prices)) {
      
      data_selected()$prices %>% 
        filter(date >= (Sys.Date() - years(2))) %>% 
        dplyr::group_by(symbol) %>% 
        dplyr::mutate(performance = (adjusted / adjusted[1L] - 1)) %>% 
        ggplot(aes(x = date, y = performance)) +
        geom_line(aes(color = symbol)) +
        geom_label(data = data_selected()$dividends,
                   aes(y = 0, x = Ex_dividend_date, label = paste0(Currency, Value)),
                   alpha = 0.4) +
        scale_y_continuous(labels = percent) +
        scale_x_date(date_labels = "%d.%m.%Y") +
        labs(title = "", x = "") +
        theme_bw()
        
    }
    
  })
  
  output$holdingsplot <- renderPlotly({
    req(data_selected())
    
    if (!is.na(data_selected()$holdings)) {
      if (nrow(data_selected()$holdings) > 0) {
        data_selected()$holdings %>% 
          bind_rows(tibble(Company = "other", holding_num = 1 - sum(.$holding_num), holding = percent(holding_num, accuracy = 0.01))) %>% 
          plot_ly(labels=~Company, values = ~holding_num, type = 'pie',
                  textposition = 'inside',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~Company,
                  marker = list(
                    line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE)
        
        print(data_selected()$dividends)
      }
    }
  })
  
}

shinyApp(ui, server)
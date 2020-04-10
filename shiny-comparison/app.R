source("setup.R")
source("data.R")

## 1. UI ----
ui <- fluidPage(
  theme = "style.css",
  useShinydashboard(),
  
  ## 1.1. first row ----
  fluidRow(
    # column(1,
    #        actionButton(inputId = "reload_fund_list", label = "reload funds list"),
    #        checkboxInput(inputId = "activate_search_datatable", label = "search/filter funds", value = F)
    #        ),
    column(10,
           checkboxGroupButtons(
             inputId = "fund_options",
             label = "",
             choices = fund_options_labels,
             status = "danger",
             individual = T,
             size = "xs",
             selected = fund_options_labels[2]
           ),
           div(style = "margin-top:-10px"),
           radioGroupButtons(
             inputId = "fund_categories",
             # label = "",
             status = "danger",
             choices = fund_categories_labels,
             individual = T,
             size = "xs",
             selected = fund_categories_labels[2]),
           dataTableOutput("fund_list_dt")
           ),
    column(2, "selected"
           )
    ),
  ## 1.2. second row ----
  fluidRow(
    column(2,
           radioGroupButtons(
             inputId = "plot_period",
             label = "Grafik:",
             choices = c("akt. Jahr", "2 Jahre", "5 Jahre", "max."),
             direction = "vertical",
             size = "sm",
             status = "primary",
             selected = "akt. Jahr"
             # individual = T
           ),
           materialSwitch(
             inputId = "performance_price",
             label = "%-Performance",
             value = TRUE,
             right = T,
             status = "primary"),
           materialSwitch(
             inputId = "currency_chf",
             label = "in CHF",
             value = TRUE, 
             right = T,
             status = "primary"),
           materialSwitch(
             inputId = "show_dividends",
             label = "Dividenden anzeigen",
             value = F, 
             right = T,
             status = "primary"),
           checkboxGroupButtons(
             inputId = "comparison_index",
             label = "Vergleichsindex",
             choices = c("SMI", "SPI"),
             status = "danger",
             individual = T,
             size = "sm",
             selected = fund_options_labels[2]
           ),
           ),
    column(6,
           plotOutput("main_plot")
           ),
    column(4, tabsetPanel(type = "tabs",
                          tabPanel("Holdings", plotOutput("holdings_plot")),
                          tabPanel("Summary", "summary"))),
    column(4, 
           "stats selected")
    ),
  ## 1.3. third row ----
  fluidRow(
    column(12,
           "table")
  )
)
  
# 2. server ----
server <- function(input, output, session) {
  
  # ## 2.0.1. fund list filtering ----
  fund_list_reactive <- reactive({
    f <- fundlist 
    
    if ("nur CHF" %in% input$fund_options) {
      f <- f %>% 
        filter(`Trading currency` == "CHF")
    }
    
    if ("ETFs" %in% input$fund_options) {
      f <- f %>% 
        filter(`Product type` == "Exchange Traded Funds")
    }
    
    if ("Replikation physisch" %in% input$fund_options) {
      f <- f %>% 
        filter(`Replication method` %in%  c("physical", "Physical"))
    }
    
    if ("mit Dividenden" %in% input$fund_options) {
      f <- f %>% 
        filter(dividends_nrow > 0)
    }
    
    if (input$fund_categories == "Schweiz") {
      f <- f %>% 
        filter(`Investment region` == "Switzerland",
               str_detect(`Asset class`, "Equity"))
    }
    
    if (input$fund_categories == "Global") {
      f <- f %>% 
        filter(`Investment region` == "Global",
               str_detect(`Asset class`, "Equity"))
    }
    
    if (input$fund_categories == "Dividend") {
      f <- f %>% 
        filter(str_detect(Name, "Dividend|DIVIDEND|dividend"),
               str_detect(`Asset class`, "Equity"))
    }
    
    if (input$fund_categories == "Value") {
      f <- f %>% 
        filter(str_detect(Name, "Value|VALUE|value"),
               str_detect(`Asset class`, "Equity"))
    }
    
    if (input$fund_categories == "Growth") {
      f <- f %>% 
        filter(str_detect(Name, "Growth|GROWTH|growth"),
               str_detect(`Asset class`, "Equity"))
    }
    
    if (input$fund_categories == "Momentum") {
      f <- f %>% 
        filter(str_detect(Name, "Momentum|MOMENTUM|momentum"),
               str_detect(`Asset class`, "Equity"))
    }
    
    if (input$fund_categories == "Tech") {
      f <- f %>% 
        filter(str_detect(Name, "Tech|TECH|tech"),
               str_detect(`Asset class`, "Equity"))
    }
    
    if (input$fund_categories == "Emerging Markets") {
      f <- f %>% 
        filter(`Investment region` == "Emerging Markets",
               str_detect(`Asset class`, "Equity"))
    }
    
    if (input$fund_categories == "Obligationen") {
      f <- f %>% 
        filter(`Asset class` == "Fixed Income")
    }
    
    if (input$fund_categories == "Gold") {
      f <- f %>% 
        filter(str_detect(Name, "Gold |GOLD |gold "),
               `Asset class` == "Commodities",
               prices_nrow > 1)
    }
    
    if (input$fund_categories == "Rohstoffe") {
      f <- f %>% 
        filter(`Asset class` == "Commodities")
    }
    
    if (input$fund_categories == "Immobilien") {
      f <- f %>% 
        filter(`Asset class` == "Real Estate")
    }
    
    if (input$fund_categories == "diverse Themen") {
      f <- f %>% 
        filter(`Asset class` == "Equity Themes")
    }
    
    if (input$fund_categories == "diverse Strategien") {
      f <- f %>% 
        filter(`Asset class` == "Equity Strategy")
    }

    f
  })
  
  ## 2.0.2. data of the fund selected  ----
  fund_selected_reactive <- reactive({
    
    if (!is.null(input$fund_list_dt_rows_selected)) {
      f <- fund_list_reactive()[input$fund_list_dt_rows_selected, ]
      
      prices <- get_six_prices_cache(f[["ISIN"]], f[["Trading currency"]])
      
      dividends <- get_six_dividends_cache(f[["ISIN"]],
                                           currency = f[["Trading currency"]])
      
      details <- get_six_details_cache(f[["ISIN"]], 
                                       currency = f[["Trading currency"]])
      
          if (nrow(prices) < 1) {
            holdings <- NA
          } else {
            holdings <- get_holdings_cache(paste0(f[["Symbol"]], ".SW"))
          }
      
      if (input$plot_period == "akt. Jahr") {
        pricesChart <- prices %>% 
          filter(year(Date) == year(Sys.Date()))
      } else if (input$plot_period == "2 Jahre") {
        pricesChart <- prices %>% 
          filter(Date >= (floor_date(Sys.Date(), "month") - years(2)))
      } else if (input$plot_period == "5 Jahre") {
        pricesChart <- prices %>% 
          filter(Date >= (floor_date(Sys.Date(), "month") - years(5)))
      } else if (input$plot_period == "max.") {
        pricesChart <- prices
      }
      
      out <- list(ISIN = f[["ISIN"]],
                  TradingCurrency = f[["Trading currency"]],
                  prices = prices,
                  pricesChart = pricesChart,
                  dividends = dividends,
                  details = details,
                  holdings = holdings)

    } else {
      out <- list()
    }
    
    out
  })
  
  ## 2.1. list of funds ----
  output$fund_list_dt <- renderDataTable({
   fund_list_reactive() %>%
     list_funds()
   }, server = F)
  
  ## 2.2. main plot ----
  output$main_plot <- renderPlot({

    req(fund_selected_reactive())

    prices <- fund_selected_reactive()$pricesChart
    currency <- fund_selected_reactive()$TradingCurrency
    dividends <- fund_selected_reactive()$dividends 
    
    if (!is.null(dividends)) {
      dividends <- dividends %>% 
        filter(Ex_dividend_date >= min(prices$Date),
               Ex_dividend_date <= max(prices$Date))
    }
    
    if (is_tibble(prices)) {
      if (nrow(prices) > 0) {
        
        if (currency != "CHF" & isTRUE(input$currency_chf)) {
          prices <- prices %>% 
            left_join(get_exchange_rate_cache(currency, "CHF") %>% rename(rate = 2), 
                      by = "Date") %>% 
            mutate(rate = na.locf(rate),
                   Close = Close*rate)
          
          if (nrow(dividends) > 0) {
            dividends <- dividends %>% 
              left_join(get_exchange_rate_cache(dividends$Currency[1], "CHF") %>% rename(rate = 2), 
                        by = c("Ex_dividend_date" = "Date")) %>% 
              mutate(rate = na.locf(rate),
                     Value = Value*rate)
          }

          currency <- "CHF"
        }
        
        if (input$performance_price) {
          prices_performance <- prices %>%
            dplyr::group_by(ISIN) %>%
            dplyr::mutate(performance = (Close / Close[1L] - 1)) %>%
            ungroup() %>%
            filter(!is.na(performance))
          
          p <- prices_performance %>% 
            ggplot(aes(x = Date, y = performance)) +
            geom_line(aes(color = ISIN)) +
            scale_y_continuous(labels = percent) +
            labs(title = "", x = "") +
            theme_bw()
          
          if (nrow(dividends) > 0) {
            dividends <- dividends %>%
              left_join(prices_performance %>% select(Date, Close, performance), by = c("Ex_dividend_date" = "Date")) %>%
              mutate(Value = Value/Close,
                     Value_label = percent(Value, accuracy = 0.01),
                     position_y = performance)
          }

        } else {
          p <- prices %>%
            ggplot(aes(x = Date, y = Close)) +
            geom_line(aes(color = ISIN)) +
            scale_y_continuous(labels = number) +
            labs(title = "", x = "", y = paste("closing price", currency)) +
            theme_bw()
          
          if (nrow(dividends) > 0) {
            dividends <- dividends %>%
              left_join(prices %>% select(Date, Close), by = c("Ex_dividend_date" = "Date")) %>%
              mutate(Value_label = paste(Currency, number(Value, accuracy = 0.01)),
                     position_y = Close)
          }
        }

        if (nrow(dividends) > 0 & input$show_dividends) {
          p <- p +
            geom_vline(data = dividends,
                       aes(xintercept = Ex_dividend_date)) +
            geom_label(data = dividends,
                       aes(y = position_y, x = Ex_dividend_date, label = Value_label),
                       alpha = 1)
        }
        
        if (input$plot_period == "akt. Jahr") {
          p <- p + 
            scale_x_date(limits = c(floor_date(Sys.Date(), "year"), Sys.Date()))
        } else if (input$plot_period == "2 Jahre") {
          p <- p + 
            scale_x_date(limits = c(Sys.Date() - years(2), Sys.Date()))
        } else if (input$plot_period == "5 Jahre") {
          p <- p + 
            scale_x_date(limits = c(Sys.Date() - years(5), Sys.Date()))
        } else if (input$plot_period == "max.") {
          p <- p + 
            scale_x_date()
        }
        
        p
        
      } else {
        plot_exception("no data is found")
      }
    } else {
      plot_exception("no data is found")
    }
  })
  
  ## 2.3. holdings plot ----
  output$holdings_plot <- renderPlot({
    req(fund_selected_reactive())

    holdings <- fund_selected_reactive()$holdings

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
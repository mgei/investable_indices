source("setup.R")
source("data.R")

## 1. UI ----
ui <- fluidPage(
  theme = "style.css",
  useShinydashboard(),
  
  ## 1.1. first row ----
  fluidRow(
    column(1,
           actionButton(inputId = "reload_fund_list", label = "reload funds list"),
           checkboxInput(inputId = "activate_search_datatable", label = "search/filter funds", value = F)
           ),
    column(9,
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
             label = "xx",
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
             label = "im CHF",
             value = TRUE, 
             right = T,
             status = "primary")
           ),
    column(6,
           plotOutput("main_plot")
           ),
    column(4, tabsetPanel(type = "tabs",
                          tabPanel("Holdings", plotOutput("holdingsplot")),
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
  
  # ## 2.0. reactives ----
  fund_list_reactive <- reactive({
    f <- fundlist 
    
    if ("nur vorhandene Daten" %in% input$fund_options) {
      f <- f %>% 
        filter(prices_nrow > 1,
               year(prices_max_date) == year(Sys.Date()))
    }
    
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
        filter(str_detect(Name, "Gold|GOLD|gold"),
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
  
  fund_selected_reactive <- reactive({
    
    if (!is.null(input$fund_list_dt_rows_selected)) {
      f <- fund_list_reactive()[input$fund_list_dt_rows_selected, ]
      
      prices <- get_prices_cache(paste0(f[["Symbol"]], ".SW"))
      
      dividends <- get_six_dividends_cache(f[["ISIN"]],
                                           currency = f[["Trading currency"]])
      
      details <- get_six_details_cache(f[["ISIN"]], 
                                       currency = f[["Trading currency"]])
      
          if (nrow(prices) < 1) {
            holdings <- NA
          } else {
            holdings <- get_holdings_cache(paste0(f[["Symbol"]], ".SW"))
          }
      
      out <- list(ISIN = f[["ISIN"]],
                  TradingCurrency = f[["Trading currency"]],
                  prices = prices,
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
  
  
  
  output$main_plot <- renderPlot({

    req(fund_selected_reactive())

    prices <- fund_selected_reactive()$prices

    if (is_tibble(prices)) {
      if (nrow(prices) > 0) {
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

        } else {
          p <- prices %>%
            filter(date >= (Sys.Date() - years(2))) %>%
            ggplot(aes(x = date, y = close)) +
            geom_line(aes(color = symbol)) +
            scale_y_continuous(labels = number) +
            scale_x_date(date_labels = "%d.%m.%Y") +
            labs(title = "", x = "", y = paste("closing price", fund_selected_reactive()$TradingCurrency)) +
            theme_bw()
        }


        dividends <- fund_selected_reactive()$dividends %>%
          filter(Ex_dividend_date >= min(prices$date),
                 Ex_dividend_date <= max(prices$date))

        if (nrow(dividends) > 0) {
          p <- p +
            geom_label(data = fund_selected_reactive()$dividends,
                       aes(y = 0, x = Ex_dividend_date, label = paste0(Currency, Value)),
                       alpha = 0.4)
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
  # 
  # output$holdingsplot <- renderPlot({
  #   req(data_selected())
  #   
  #   holdings <- data_selected()$holdings
  #   
  #   if (is_tibble(holdings)) {
  #     if (nrow(holdings) > 0) {
  #       
  #       d <- holdings %>% 
  #         bind_rows(tibble(Company = "_other", holding_num = 1 - sum(.$holding_num)) %>% 
  #                     mutate(holding_num = max(holding_num, 0), holding = percent(holding_num, accuracy = 0.01))) %>% 
  #         ggplot(aes(x = "", y = holding_num, fill = Company)) +
  #         geom_col() +
  #         geom_text_repel(aes(label = holding), position = position_stack(vjust = .5), size = 4) +
  #         coord_polar("y", start = 0) +
  #         labs(title = "Fund holdings",
  #              fill = "") +
  #         theme_void() +
  #         theme(legend.position = "bottom",
  #               legend.text = element_text(size = 9))
  #       
  #       d
  #       
  #     } else {
  #       plot_exception("no holdings data is found")
  #     }
  #   } else {
  #     plot_exception("no holdings data is found")
  #   }
  # })
  
}

shinyApp(ui, server)
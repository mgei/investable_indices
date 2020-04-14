source("setup.R")
source("data.R")

## 1. UI ----
ui <- fluidPage(
  theme = "style.css",
  useShinydashboard(),
  
  ## 1.1. first row ----
  column(10,
  fluidRow(
    # column(1,
    #        actionButton(inputId = "reload_fund_list", label = "reload funds list"),
    #        checkboxInput(inputId = "activate_search_datatable", label = "search/filter funds", value = F)
    #        ),
    column(1, style='border-right: 2px solid red',
           h3("Fund/ETF catalog")),
    column(8,
           div(style = "margin-top:-10px"),
           checkboxGroupButtons(
             inputId = "fund_options",
             label = "",
             choices = fund_options_labels,
             status = "danger",
             individual = T,
             size = widget_size,
             selected = fund_options_labels[2]
           ),
           div(style = "margin-top:-10px"),
           radioGroupButtons(
             inputId = "fund_categories",
             # label = "",
             status = "danger",
             choices = c(fund_categories_labels, "Suche Volltext"),
             individual = T,
             size = widget_size,
             selected = fund_categories_labels[2]),
           div(style = "margin-top:-10px"),
           ),
    column(3,
           uiOutput("text_search"))
    ),
  fluidRow(
    column(12,
           dataTableOutput("fund_list_dt"))
  ),
  ## 1.2. second row ----
  fluidRow(
    column(2,
           radioGroupButtons(
             inputId = "plot_period",
             label = "Grafik:",
             choices = c("akt. Jahr", "12 Mte", "2 Jahre", "5 Jahre", "max."),
             direction = "vertical",
             size = widget_size,
             status = "danger",
             selected = "2 Jahre"
             # individual = T
           ),
           materialSwitch(
             inputId = "performance_price",
             label = "%-Performance",
             value = TRUE,
             right = T,
             status = "danger"),
           materialSwitch(
             inputId = "currency_chf",
             label = "in CHF",
             value = TRUE, 
             right = T,
             status = "danger"),
           materialSwitch(
             inputId = "show_dividends",
             label = "Dividenden anzeigen",
             value = F, 
             right = T,
             status = "danger"),
           checkboxGroupButtons(
             inputId = "comparison_index",
             label = "Vergleichsindex",
             choices = c("SMI", "SPI"),
             status = "danger",
             individual = T,
             size = widget_size,
             selected = "SMI"
           )
           ),
    column(6,
           plotOutput("main_plot")
           # plotOutput("dividend_plot")
           ),
    column(4, plotOutput("holdings_plot"))
    ),
  ## 1.3. third row ----
  fluidRow(
    column(2
           ),
    column(7,
           dataTableOutput("performance_table")),
    column(3, 
           div(style = "margin-top:50px"),
           plotOutput("drawdowns_plot"))
  )),
  column(2,
         tableOutput("details_table"),
         hr(),
         div(style = "margin-top:-10px"),
         uiOutput("stats_TR_showhide"),
         div(style = "margin-top:-10px"),
         tableOutput("stats_table"),
         hr(),
         tableOutput("dividends_table"),
         hr(),
         uiOutput("external_links")
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
    
    if (input$fund_categories == "Low Volatility") {
      f <- f %>% 
        filter(str_detect(Name, "Low Vola"),
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
    
    if (input$fund_categories == "aktives Management") {
      f <- f %>% 
        filter(`Management style` == "active")
    }
    
    if (input$fund_categories == "Suche Volltext") {
      
      searchterm <- input$search_fulltext
      
      if (!is.null(searchterm)) {
        if (str_length(searchterm) > 1) {
          f <- f %>% 
            filter(str_detect(Name, regex(input$search_fulltext, ignore_case = T)))
        }
      }
    }

    f
  })
  
  ## 2.0.2. data of the fund selected  ----
  fund_selected_reactive <- reactive({
    
    if (!is.null(input$fund_list_dt_rows_selected)) {
      
      f <- fund_list_reactive()[input$fund_list_dt_rows_selected, ]
      
      details <- get_six_details_cache(f[["ISIN"]], 
                                       currency = f[["Trading currency"]])
      
      prices <- get_six_prices_cache(f[["ISIN"]], f[["Trading currency"]]) %>% 
        mutate(Currency = f[["Trading currency"]])
      
      if (nrow(prices) > 0) {
        if (prices$Currency[1] != "CHF") {
          pricesCHF <- prices %>% 
            left_join(get_exchange_rate_cache(.$Currency[1], "CHF") %>% dplyr::rename(rate = 2), 
                      by = c("Date")) %>% 
            mutate(rate = na.locf(rate),
                   Close = Close*rate,
                   Currency = "CHF") %>% 
            select(-rate)
        } else {
          pricesCHF <- prices %>% 
            mutate(Currency = "CHF")
        }
      } else {
        pricesCHF <- prices %>% 
          mutate(Currency = "CHF")
      }
      
      dividends <- get_six_dividends_cache(f[["ISIN"]],
                                           currency = f[["Trading currency"]])
      
      if (nrow(dividends) > 0) {
        if (dividends$Currency[1] != "CHF") {
          dividendsCHF <- dividends %>% 
            left_join(get_exchange_rate_cache(.$Currency[1], "CHF") %>% dplyr::rename(rate = 2), 
                      by = c("Ex_dividend_date" = "Date")) %>% 
            mutate(rate = na.locf(rate),
                   Value = Value*rate,
                   Currency = "CHF")
        } else {
          dividendsCHF <- dividends
        }
      } else {
        dividendsCHF <- dividends
      }
      
      holdings <- get_holdings_cache(paste0(f[["Symbol"]], ".SW"))
      
      returnsCHF <- pricesCHF %>% select(-Currency) %>% 
        left_join(dividendsCHF %>% select(-ISIN, -Currency), by = c("Date" = "Ex_dividend_date")) %>% 
        mutate(CloseInclDiv = if_else(!is.na(Value), Close+Value, Close), 
               Ra = Close/lag(Close) - 1,
               RaTR = CloseInclDiv/lag(Close) - 1,
               Dividend12 = roll_sum(Value, 250, align = "right", fill = NA, na.rm = T),
               Dividend24 = roll_sum(Value, 500, align = "right", fill = NA, na.rm = T),
               DYield12 = Dividend12/Close,
               DYield24 = Dividend24*0.5/Close)
      
      out <- list(ISIN = f[["ISIN"]],
                  TradingCurrency = f[["Trading currency"]],
                  Symbol = f[["Symbol"]],
                  prices = prices,
                  pricesCHF = pricesCHF,
                  returnsCHF = returnsCHF,
                  dividends = dividends,
                  dividendsCHF = dividendsCHF,
                  details = details,
                  holdings = holdings)

    } else {
      out <- list()
    }
    
    out
  })
  
  ## 2.0.3. stats of the fund selected ----
  fund_selected_stats_reactive <- reactive({
    req(fund_selected_reactive())
    req(input$plot_period)
    # req(input$stats_TR)
    
    if (!is.null(input$stats_TR)) {
      returnsCHF <- fund_selected_reactive()$returnsCHF
      
      if (!is.null(returnsCHF)) {
        if (nrow(returnsCHF) > 0) {
          if (input$plot_period == "akt. Jahr") {
            returnsCHF <- returnsCHF %>% 
              filter(year(Date) == year(Sys.Date()))
          } else if (input$plot_period == "12 Mte") {
            returnsCHF <- returnsCHF %>% 
              filter(Date >= (floor_date(Sys.Date(), "month") - months(12)))
          } else if (input$plot_period == "2 Jahre") {
            returnsCHF <- returnsCHF %>% 
              filter(Date >= (floor_date(Sys.Date(), "month") - years(2)))
          } else if (input$plot_period == "5 Jahre") {
            returnsCHF <- returnsCHF %>% 
              filter(Date >= (floor_date(Sys.Date(), "month") - years(5)))
          } else if (input$plot_period == "max.") {
            returnsCHF <- returnsCHF
          }
          
          if (input$stats_TR) {
            rets <- returnsCHF %>% 
              tq_performance(Ra = RaTR, Rb = NULL, performance_fun = table.AnnualizedReturns, geometric = F)
            
            VaR <- returnsCHF %>% 
              tq_performance(Ra = RaTR, Rb = NULL, performance_fun = VaR, method = "gaussian", p = 0.95) %>% 
              mutate(VaR = VaR * sqrt(250))
          } else {
            rets <- returnsCHF %>% 
              tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns, geometric = F)
            
            VaR <- returnsCHF %>% 
              tq_performance(Ra = Ra, Rb = NULL, performance_fun = VaR, method = "gaussian", p = 0.95) %>% 
              mutate(VaR = VaR * sqrt(250))
          }
          
          stats <- bind_cols(rets, VaR) %>% 
            mutate(helper = 1) %>% 
            pivot_longer(names_to = "Name", values_to = "Value", -helper) %>% 
            select(-helper)
            
          stats
        }
      }
    }
  })
  
  ## 2.0.4. comparison index ----
  comparison_index_reactive <- reactive({
    out <- tibble()
    if ("SMI" %in% input$comparison_index) {
      smi <- get_six_index_cache("SMI") %>% 
        dplyr::rename(Close = SMI) %>% 
        mutate(ISIN = "SMI")
      smi_tr <- get_six_index_cache("SMIC") %>% 
        dplyr::rename(Close = SMIC) %>% 
        mutate(ISIN = "SMIC")
      
      out <- bind_rows(out, 
                       smi, 
                       smi_tr)
    } 
    if ("SPI" %in% input$comparison_index) {
      spi <- get_six_index_cache("SPIX") %>% 
        dplyr::rename(Close = SPIX) %>% 
        mutate(ISIN = "SPIX")
      spi_tr <- get_six_index_cache("SXGE") %>% 
        dplyr::rename(Close = SXGE) %>% 
        mutate(ISIN = "SXGE")
      
      out <- bind_rows(out, 
                       spi, 
                       spi_tr)
    }
    
    out <- out %>% 
      arrange(ISIN, Date) %>% 
      group_by(ISIN) %>% 
      mutate(Close = na.locf(Close),
             Ra = Close/lag(Close)-1,
             RaTR = Ra) %>% 
      ungroup()
    
    out
  })
  
  ## 2.1. list of funds ----
  output$fund_list_dt <- renderDataTable({
   fund_list_reactive() %>%
     list_funds(fontsize = fontsize)
   }, server = F)
  
  ## 2.1.a. list of funds fulltext search ----
  output$text_search <- renderUI({
    req(input$fund_categories)
    
    if (input$fund_categories == "Suche Volltext") {
      searchInput(
        inputId = "search_fulltext", 
        label = "", 
        placeholder = "SUCHE", 
        btnSearch = icon("search"), 
        btnReset = icon("remove")
      )
    }
  })
  
  
  ## 2.2. main plot ----
  output$main_plot <- renderPlot({

    req(fund_selected_reactive())
    
    if (input$currency_chf) {
      prices <- fund_selected_reactive()$pricesCHF
      dividends <- fund_selected_reactive()$dividendsCHF
    } else {
      prices <- fund_selected_reactive()$prices
      dividends <- fund_selected_reactive()$dividends
    }
    
    currency <- fund_selected_reactive()$TradingCurrency
    
    if (is_tibble(prices)) {
      if (nrow(prices) > 0) {
        
        if (input$performance_price & input$currency_chf) {
          comparison <- comparison_index_reactive()
          if (!is.null(comparison)) {
            if (nrow(comparison) > 0) {
              prices <- bind_rows(prices,
                                  comparison)
            }
          }
        }
        
        from_date_fund <- min(fund_selected_reactive()$prices$Date)
        
        if (input$plot_period == "akt. Jahr") {
          prices <- prices %>% 
            filter(Date >= max(from_date_fund, floor_date(Sys.Date(), "year")))
          
          if (!is.null(dividends)) {
            dividends <- dividends %>% 
              filter(Ex_dividend_date >= max(from_date_fund, floor_date(Sys.Date(), "year")))
          }
        } else if (input$plot_period == "12 Mte") {
          prices <- prices %>% 
            filter(Date >= max(from_date_fund, (floor_date(Sys.Date(), "month") - months(12))))
          
          if (!is.null(dividends)) {
            dividends <- dividends %>% 
              filter(Ex_dividend_date >= max(from_date_fund, (floor_date(Sys.Date(), "month") - months(12))))
          }
        } else if (input$plot_period == "2 Jahre") {
          prices <- prices %>% 
            filter(Date >= max(from_date_fund, (floor_date(Sys.Date(), "month") - years(2))))
          
          if (!is.null(dividends)) {
            dividends <- dividends %>% 
              filter(Ex_dividend_date >= max(from_date_fund, (floor_date(Sys.Date(), "month") - years(2))))
          }
        } else if (input$plot_period == "5 Jahre") {
          prices <- prices %>% 
            filter(Date >= max(from_date_fund, (floor_date(Sys.Date(), "month") - years(5))))
          
          if (!is.null(dividends)) {
            dividends <- dividends %>% 
              filter(Ex_dividend_date >= max(from_date_fund, (floor_date(Sys.Date(), "month") - years(5))))
          }
        } else if (input$plot_period == "max.") {
          prices <- prices %>% 
            filter(Date >= from_date_fund)
          
          if (!is.null(dividends)) {
            dividends <- dividends %>% 
              filter(Ex_dividend_date >= from_date_fund)
          }
        }
        
        if (input$performance_price) {
          prices_performance <- prices %>%
            dplyr::group_by(ISIN) %>%
            dplyr::mutate(performance = (Close / Close[1L] - 1)) %>%
            ungroup() %>%
            filter(!is.na(performance))
          
          p <- prices_performance %>% 
            ggplot(aes(x = Date, y = performance)) +
            geom_line(aes(color = ISIN, size = (ISIN %in% c("SMI", "SMIC", "SXGE", "SPIX")))) +
            scale_y_continuous(labels = percent, breaks = seq(-1, 10, by = 0.05)) +
            scale_size_discrete(range = c(1, 0.3), guide = F) +
            labs(title = "", x = "", y = "", color = "") +
            theme_bw()
          
          if (input$show_dividends) {
            if (nrow(dividends) > 0) {
              dividends <- dividends %>%
                left_join(prices_performance %>% filter(!(ISIN %in% c("SMI", "SMIC", "SXGE", "SPIX"))) %>% select(Date, Close, performance), 
                          by = c("Ex_dividend_date" = "Date")) %>%
                mutate(Value = Value/Close,
                       Value_label = percent(Value, accuracy = 0.01),
                       position_y = performance)
            }
          }

        } else {
          p <- prices %>%
            filter(!is.na(Close)) %>% 
            ggplot(aes(x = Date, y = Close)) +
            geom_line(aes(color = ISIN), size = 1) +
            scale_y_continuous(labels = number) +
            labs(title = "", x = "", y = prices$Currency[1]) +
            theme_bw()
          
          if (nrow(dividends) > 0) {
            dividends <- dividends %>%
              left_join(prices %>% filter(!(ISIN %>% c("SMI", "SMIC", "SXGE", "SPIX"))) %>% select(Date, Close), 
                        by = c("Ex_dividend_date" = "Date")) %>%
              mutate(Value_label = paste("CURRENCY", number(Value, accuracy = 0.01)),
                     position_y = Close)
          }
        }

        if (input$show_dividends) {
          if (nrow(dividends) > 0) {
            p <- p +
              geom_vline(data = dividends,
                         aes(xintercept = Ex_dividend_date),
                         alpha = 0.4) +
              geom_label(data = dividends,
                         aes(y = position_y, x = Ex_dividend_date, label = Value_label),
                         alpha = 1)
          } else {
            p <- p +
              labs(title = "no dividends!")
          }
        } 
        
        p +
          scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), length.out = 10),
                       date_labels = "%d.%m.%y") +
          theme(legend.position = c(0.7, 0.2),
                legend.background = element_rect(linetype = 1, size = 0.2, color = "black"),
                axis.text = element_text(size = 15), 
                axis.title = element_text(size = 15),
                legend.text = element_text(size = 15),
                legend.title = element_text(size = 1))
        
      } else {
        plot_exception("no data selected or found", type = "none")
      }
    } else {
      plot_exception("no data selected or found", type = "none")
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
          filter(holding_num >= 0.01) %>% 
          mutate(Company = str_sub(Company, 1, 15)) %>% 
          ggplot(aes(x = 1, y = holding_num, fill = Company)) +
          geom_col(alpha = 0.8) +
          geom_text_repel(aes(label = holding), position = position_stack(vjust = .5), size = 4) +
          coord_polar("y", start = 0) +
          labs(title = "",
               fill = "") +
          scale_fill_brewer(palette = "Set3") +
          guides(colour=guide_legend(ncol=1)) +
          theme_void() +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 14),
                legend.title = element_text(size = 1))

        d

      } else {
        plot_exception("no data selected or found", type = "none")
      }
    } else {
      plot_exception("no data selected or found", type = "none")
    }
  })
  
  ## 2.4. details table ----
  output$details_table <- renderTable({
    req(fund_selected_reactive())
    
    details <- fund_selected_reactive()$details
    
    if (!is.null(details)) {
      details %>% 
        as_tibble() %>% 
        select(ValorSymbol = Valor_symbol, ValorNumber = Valor_number, ISIN, TradingCurrency = Trading_currency,
               Exchange, ProductType = Product_type, AssetClass = Asset_class, Domicile = Domicile_of_fund, 
               InvestmentRegion = Investment_region, ManagementStyle = Management_style, MarketExpectation = Market_expectation,
               ReplicationMethod = Replication_method, FundManager = Fund_manager, Underlying, FundCurrency = Fund_currency,
               ManagementFee = Management_fee) %>% 
        mutate(helper = 1) %>% 
        pivot_longer(names_to = "Name", values_to = "Value", -helper) %>% 
        select(-helper)
    }
  }, colnames = F, spacing = "xs", hover = T)
  
  ## 2.5. stats table ----
  output$stats_table <- renderTable({
    req(fund_selected_stats_reactive())
    
    fund_selected_stats_reactive() %>% 
      mutate_if(is.numeric, percent) %>% 
      dplyr::rename(" " = 1)
  }, colnames = F, spacing = "xs", hover = T)
  
  ## 2.5.a. stats TR switch ----
  output$stats_TR_showhide <- renderUI({
    req(input$fund_list_dt_rows_selected)
    
    if (!is.null(input$fund_list_dt_rows_selected)) {
      prettyCheckbox(
        inputId = "stats_TR",
        label = "Berechnung inkl. Dividenden", 
        value = TRUE,
        status = "danger",
        shape = "curve"
      )
    }
  })
  
  ## 2.6. dividends table ----
  output$dividends_table <- renderTable({
    returnsCHF <- fund_selected_reactive()$returnsCHF
    
    if (!is.null(returnsCHF)) {
      returnsCHF %>% 
        filter(Date == max(Date)) %>% 
        select(Dividend12, Dividend24,
               DYield12, DYield24) %>% 
        mutate(Dividend12 = number(Dividend12, big.mark = "'", accuracy = 0.01), 
               Dividend24 = number(Dividend24, big.mark = "'", accuracy = 0.01),
               DYield12 = percent(DYield12, accuracy = 0.1), DYield24 = percent(DYield24, accuracy = 0.1),
               helper = 1) %>% 
        pivot_longer(names_to = "Name", values_to = "Value", -helper) %>% 
        select(-helper)
    }
  }, colnames = F, spacing = "xs", hover = T)
  
  # ## 2.7. dividends plot ----
  # output$dividend_plot <- renderPlot({
  #   returnsCHF <- fund_selected_reactive()$returnsCHF
  #   
  #   if (is_tibble(returnsCHF)) {
  #     if (nrow(returnsCHF) > 0) {
  #       if (input$plot_period == "akt. Jahr") {
  #         prices <- prices %>% 
  #           filter(year(Date) == year(Sys.Date()))
  #         
  #         if (!is.null(dividends)) {
  #           dividends <- dividends %>% 
  #             filter(year(Ex_dividend_date) == year(Sys.Date()))
  #         }
  #       } else if (input$plot_period == "12 Mte") {
  #         prices <- prices %>% 
  #           filter(Ex_dividend_date >= (floor_date(Sys.Date(), "month") - months(12)))
  #         
  #         if (!is.null(dividends)) {
  #           dividends <- dividends %>% 
  #             filter(Ex_dividend_date >= (floor_date(Sys.Date(), "month") - months(12)))
  #         }
  #       } else if (input$plot_period == "2 Jahre") {
  #         prices <- prices %>% 
  #           filter(Date >= (floor_date(Sys.Date(), "month") - years(2)))
  #         
  #         if (!is.null(dividends)) {
  #           dividends <- dividends %>% 
  #             filter(Ex_dividend_date >= (floor_date(Sys.Date(), "month") - years(2)))
  #         }
  #       } else if (input$plot_period == "5 Jahre") {
  #         prices <- prices %>% 
  #           filter(Date >= (floor_date(Sys.Date(), "month") - years(5)))
  #         
  #         if (!is.null(dividends)) {
  #           dividends <- dividends %>% 
  #             filter(Ex_dividend_date >= (floor_date(Sys.Date(), "month") - years(5)))
  #         }
  #       }
  #   
  #   if (!is.null(returnsCHF)) {
  #     returnsCHF %>% 
  #       ggplot(aes(x = Date, y = DYield12)) +
  #       geom_line()
  #     
  #   }
  # })
  
  ## 2.8. performance table ----
  output$performance_table <- renderDataTable({
    returnsCHF <- fund_selected_reactive()$returnsCHF
    
    if (!is.null(returnsCHF)) {
      comparison <- comparison_index_reactive()
      cap_year <- (year(Sys.Date())-10)
      if (!is.null(comparison)) {
        if (nrow(comparison) > 0) {
          returnsCHF <- bind_rows(returnsCHF,
                                  comparison %>% 
                                    filter(Date >= min(returnsCHF$Date),
                                           year(Date) >= cap_year) %>% 
                                    mutate(type = "index"))
        }
      }
      
      performance_years <- returnsCHF %>% 
        filter(year(Date) >= cap_year) %>%
        group_by(ISIN, 
                 year = year(Date) %>% as.character()) %>% 
        summarise(Return = prod(1+Ra)-1, 
                  TotalReturn = prod(1+RaTR)-1, 
                  Volatility = sd(Ra)*sqrt(250),
                  SharpeRatio = TotalReturn/Volatility,
                  Dividendyield = sum(Value, na.rm = T)/last(Close)) %>% 
        filter(!is.na(Return))
      
      performance_12months <- returnsCHF %>% 
        filter(Date >= Sys.Date() - months(12)) %>% 
        group_by(ISIN) %>% 
        summarise(Return = prod(1+Ra, na.rm = T)-1, 
                  TotalReturn = prod(1+RaTR, na.rm = T)-1, 
                  Volatility = sd(Ra, na.rm = T)*sqrt(250),
                  SharpeRatio = TotalReturn/Volatility,
                  Dividendyield = sum(Value, na.rm = T)/last(Close)) %>% 
        mutate(year = "12Mte")
      
      x <- bind_rows(performance_years,
                performance_12months) %>% 
        ungroup() %>% 
        pivot_longer(names_to = "n", values_to = "v", -c("year", "ISIN")) %>%
        pivot_wider(names_from = year, values_from = v)
      
      bind_rows(x %>% filter(ISIN == fund_selected_reactive()$ISIN, n != "SharpeRatio") %>% mutate_if(is.numeric, percent, accuracy = 0.1),
                x %>% filter(n == "SharpeRatio") %>% mutate_if(is.numeric, number, accuracy = 0.01)) %>% 
        dplyr::rename(" " = 1) %>% 
        datatable(options = list(dom = 't', ordering = F),
                  rownames = F)
    }
  })
  
  ## 2.9. external links ----
  output$external_links <- renderUI({
    if (!is_empty(fund_selected_reactive())) {
      yahoo_link <- a("Yahoo Finance", 
                      href = paste0("https://finance.yahoo.com/quote/", fund_selected_reactive()$Symbol, ".SW"),
                      target="_blank")
      six_link <- a("SIX Swiss Exchange", 
                    href = paste0("https://www.six-group.com/exchanges/funds/security_info_en.html?id=", 
                                  fund_selected_reactive()$ISIN, fund_selected_reactive()$TradingCurrency, "4"),
                    target="_blank")
      
      nzz_link <- a("NZZ The Market", 
                    href = paste0("https://themarket.nzz.ch/suche/alle/", 
                                  fund_selected_reactive()$ISIN),
                    target="_blank")
      
      cash_link <- a("Cash.ch",
                     href = paste0("https://www.cash.ch/suche/alle/",
                                   fund_selected_reactive()$ISIN),
                     target="_blank")
      
      tagList(strong("Direktlinks:"),
              br(),
              yahoo_link,
              br(),
              six_link,
              br(),
              nzz_link,
              br(),
              cash_link)
    }
  })
  
  ## 2.10. drawdowns plot ----
  output$drawdowns_plot <- renderPlot({
    returnsCHF <- fund_selected_reactive()$returnsCHF
    
    if (!is.null(returnsCHF)) {
      returnsCHF %>% 
        filter(!is.na(RaTR)) %>% 
        mutate(drawdown = calc_drawdown(Ra)) %>% 
        ggplot(aes(x = Date, y = drawdown)) +
        geom_line(color = "red") +
        scale_x_date(date_labels = "%m.%y") +
        scale_y_continuous(labels = percent) +
        labs(x = "", y = "drawdown") +
        theme_bw()
    }
  }, height = 200L)
}

shinyApp(ui, server)
# 0. setup ----
source("setup.R")
# source("data.R")

# 1. UI ----
ui <- fluidPage(
  theme = "style.css",
  tags$head(tags$style('.btn-group{ margin-top: 5px;}')),
  useShinydashboard(),
  # fluidRow(div(style = "margin-top:10px")),
  div(style = "margin-top:10px"),
  
  column(2,
         h3("ETF Portfolio Calculator"),
         hr(style="height:4px;background-color:#286090;margin-top:10px;"),
         helpText("Please select up to 6 ETFs from the list below, or select a sample portfolio."),
         helpText("Give it some time to load, as the data is fetched from Yahoo Finance and computed on-the-fly."),
         helpText("The investment period is self-adjusting, allowing to select only the date range in which all selected ETFs have price data.",
                  "The weights and the rebalance frequency has effect on the computed portfolio."),
         pickerInput(
           inputId = "etfs",
           label = "ETFs:", 
           choices = NULL,
           multiple = T,
           width = "85%",
           options = list(
             # size = 5,
             "live-search" = TRUE,
             "live-search-style" = "startsWith",
             size = 5.5,
             # "virtual-scroll" = 10,
             # "show-content" = F,
             "live-search-placeholder" = "type a ticker symbol",
             "none-selected-text" = "select your ETFs",
             # `actions-box` = TRUE,
             "max-options" = max_etfs,
             "max-options-text" = paste0("select no more than ", max_etfs, "!")
           )),
         div(style = "margin-top:-10px"),
         actionLink("reset_picker", "reset selection"),
         # br(),
         div(style = "margin-top:10px"),
         tags$b("Sample portfolios:"),
         br(),
         actionLink("sample_allweather", "All Weather"),
         br(),
         actionLink("sample_vanguardtotal", "Vanguard Total World + Bond"),
         br(),
         actionLink("sample_USgoldbond", "S&P500 + Bond + Gold"),
         br(),
         actionLink("sample_USgoldbondemcom", "S&P500 + Bond + Gold + Emerging Mkt + Commodities"),
         div(style = "margin-top:10px"),
         sliderInput("daterange", "Investment period:",
                     min = Sys.Date() - years(20),
                     max = Sys.Date(),
                     value = c(Sys.Date() - years(10),
                               Sys.Date()),
                     timeFormat="%Y-%m-%d"),
         div(style = "margin-top:10px"),
         radioGroupButtons("rebalance_on",
                           "Rebalance on:",
                           choices = c("days", "weeks", "months", "quarters", "years"
                                       # , "never", "when it deviates by more than ..."
                           ),
                           selected = "months",
                           individual = T,
                           status = "primary",
                           size = "xs"),
         div(style = "margin-top:10px"),
         tags$b("Portfolio optimization:"),
         helpText("Optimization functionality will be coming soon.")
  ),
  column(8,
         fluidRow(
           uiOutput("sliders") #, proxy.height = "200px")
         ),
         fluidRow(
           uiOutput("etf_stats")
         ),
         fluidRow(
           hr()
         ),
         fluidRow(
           uiOutput("pf_stats"),
           uiOutput("correlations2")
           )
         ),
  column(2,
         uiOutput("correlations1"))
)

# 2. server ----
server <- function(input, output, session) {
  
  ## 2.0. reactive values ----
  subitems <- reactiveVal(character())
  
  ## 2.1. picker input ----
  updatePickerInput(session, "etfs", 
                    choices = get_IB_etflist_cache("ARCA")$symbol,
                    choicesOpt = list(
                      subtext = get_IB_etflist_cache("ARCA")$description))
  
  observeEvent(input$reset_picker, {
    updatePickerInput(session, "etfs",
                      selected = "")
  })
  
  observeEvent(input$sample_allweather, {
    updatePickerInput(session, "etfs",
                      selected = c("TLT", "VT", "IEF", "GLD", "GSG"))
    
  })
  
  observeEvent(input$sample_vanguardtotal, {
    updatePickerInput(session, "etfs",
                      selected = c("VT", "BND"))
  })
  
  observeEvent(input$sample_USgoldbond, {
    updatePickerInput(session, "etfs",
                      selected = c("SPY", "GLD", "BND"))
    
  })
  
  observeEvent(input$sample_USgoldbondemcom, {
    updatePickerInput(session, "etfs",
                      selected = c("SPY", "GLD", "BND", "VWO", "GSG"))
  })
  

  
  ## 2.2. slider inputs ----
  
  observeEvent(input$etfs, {
    # subitems(paste0("slider_", input$etfs))
    subitems(input$etfs)
  })
  
  output$sliders <- renderUI({
    req(input$etfs)
    
    lapply(subitems(), 
           function(x) { column(2, sliderInput(paste0("slider_", x), x, value = 1/length(subitems()), min = 0, max = 1)) })
  })
  
  sum_of_sliders <- reactive({
    req(input$etfs)
    
    sum <- 0
    for(s in paste0("slider_", subitems())) {
      sum <- sum + input[[s]]
    }
    sum
  })
  
  observe({
    lapply(
      subitems(),
      function(x) {
        observeEvent(input[[paste0("slider_", x)]], {
          req(sum_of_sliders())
          if (sum_of_sliders() > 1) {
            updateSliderInput(session, paste0("slider_", x), value = 1 - (sum_of_sliders() - input[[paste0("slider_", x)]]))
          }
        })
      }
    )
  })
  
  ## 2.3. rebalance ----
  
  
  ## 2.4. etf stats ----
  price_return_data <- reactive({
    req(input$etfs)
    
    x <- get_yahoo_prices_cache(symbol = input$etfs, from = input$daterange[1], to = input$daterange[2]) %>% 
      group_by(symbol) %>% 
      mutate(ret = adjusted/lag(adjusted) - 1) %>% 
      ungroup()
    
    x %>% saveRDS("d.RDS")
    
    x
    
  })
  
  observe({
    mindate <- price_return_data() %>% 
      group_by(symbol) %>% 
      summarise(mindate = min(date)) %>% 
      summarise(max(mindate)) %>% 
      pull()
    
    maxdate <- price_return_data() %>% 
      group_by(symbol) %>% 
      summarise(maxdate = max(date)) %>% 
      summarise(min(maxdate)) %>% 
      pull()
    
    updateSliderInput(session, "daterange", value = c(mindate, maxdate))
  })
  
  table_stats <- reactive({ 
    lapply(subitems(), 
           function(x) {
             bind_rows(price_return_data() %>% 
                         filter(symbol == x) %>% 
                         summarise(return_num = prod(1 + ret, na.rm = T)^(1/(n()/252)) - 1,
                                   volatility_num = sd(ret, na.rm = T)*sqrt(250),
                                   SR_num = return_num/volatility_num) %>% 
                         mutate(return = percent(return_num),
                                volatility = percent(volatility_num),
                                SR = number(SR_num, accuracy = 0.01),
                                symbol = x))
             }) %>% 
      bind_rows()
    })
  
  output$etf_stats <- renderUI({
    req(input$etfs)
    
    lapply(subitems(), 
           function(x) {
             output[[paste0("stats_tbl_", x)]] <- renderTable({
               table_stats() %>% 
                 filter(symbol == x) %>% 
                 select(-symbol, -contains("_num"))
             })
             
             output[[paste0("stats_gg_", x)]] <- renderPlotly({
               p <- price_return_data() %>% 
                 filter(symbol == x) %>% 
                 mutate(performance = adjusted/adjusted[1L] - 1) %>% 
                 ggplot(aes(x = date, y = performance)) +
                 geom_line(color = "red") +
                 scale_y_continuous(labels = percent) +
                 labs(x = "", y = "") +
                 theme_bw()
               
               ggplotly(p) %>% 
                 config(displayModeBar = F)
             })
             
             column(2,
                    tableOutput(paste0("stats_tbl_", x)), #%>% withSpinner(type = 1, proxy.height = "100px"),
                    plotlyOutput(paste0("stats_gg_", x), height = "150px")
                    ) 
             }
           )
  })
  
  output$correlations1 <- renderUI({
    req(price_return_data())
    
    if (length(subitems()) > 1) {
    
      price_return_data_wide <- price_return_data() %>%
        select(symbol, date, ret) %>%
        filter(!is.na(ret)) %>%
        pivot_wider(names_from = "symbol", values_from = "ret") %>%
        filter(!is.na(rowSums(.[-1]))) %>% 
        select(-date)
      
      corr <- price_return_data_wide %>% 
        cor()
  
      output[["corplot"]] <- renderPlotly({
        p <- ggcorrplot(corr, hc.order = T,
                   lab = TRUE, title = "Correlations",
                   type = "lower", show.diag = T,
                   ggtheme = ggplot2::theme_bw(),
                   show.legend = F)
        
        ggplotly(p) %>% 
          config(displayModeBar = F)
      })
      
      pca <- price_return_data_wide %>%
        prcomp(scale. = TRUE)
  
      output[["pcaplot"]] <- renderPlotly({
        p <- autoplot(pca, data = price_return_data_wide,
                 size = 0.2,
                 alpha = 0.2,
                 loadings = TRUE,
                 main = "PCA",
                 loadings.colour = "red",
                 loadings.label = TRUE,
                 loadings.label.size = 3,
                 loadings.label.repel = F) +
          theme_bw()
        
        ggplotly(p) %>% 
          config(displayModeBar = F)
      })
  
      column(12,
             plotlyOutput("corplot", height = "250px"), # %>% withSpinner(type = 1, proxy.height = "200px"),
             plotlyOutput("pcaplot", height = "200px")
             )
    }
  })
  
  ## 2.5. portfolio ----
  pf_weights <- reactive({
    req(subitems())
    
    w <- c()
    for(s in  paste0("slider_", subitems())) {
      w <- c(w, input[[s]])
    }
    w
  })
  
  pf_value_return_data <- reactive({
    req(input$etfs)
    req(input$rebalance_on)
    
    if (length(pf_weights()) == length(subitems())) {
      price_return_data() %>% 
        tq_portfolio(assets_col = symbol, returns_col = ret, weights = pf_weights(), rebalance_on = input$rebalance_on, 
                     col_rename = "ret") %>% 
        mutate(performance = cumprod(1+ret)-1)
    }
  })
  
  table_stats_pf <- reactive({
    req(pf_value_return_data())
    
    pf_value_return_data() %>%
      summarise(return_num = prod(1 + ret, na.rm = T)^(1/(n()/252)) - 1,
                volatility_num = sd(ret, na.rm = T)*sqrt(250),
                SR_num = return_num/volatility_num) %>%
      mutate(return = percent(return_num),
             volatility = percent(volatility_num),
             SR = number(SR_num, accuracy = 0.01),
             symbol = "Portfolio")
  }) 

  output$pf_stats <- renderUI({
    req(pf_value_return_data())
    
    des_cur_tbl <- get_IB_etflist_cache() %>% 
      filter(symbol %in% subitems()) %>% 
      mutate(des_cur = paste0(description, " (", currency, ")"))
    
    des_cur_vct <- setNames(as.character(des_cur_tbl$des_cur), des_cur_tbl$symbol)
    
    output$constitutes <- renderUI({
      lapply(subitems(), 
             function(x) {
               tagList(
                 a(x, 
                   href = paste0("https://finance.yahoo.com/quote/", x),
                   target="_blank"),
                 percent(input[[paste0("slider_", x)]]),
                 p(des_cur_vct[x], id = "grayed")
                 # br()
                 )
             }
             )
    })
    
    output[["stats_gg_pf"]] <- renderPlotly({
      req(pf_value_return_data())
      
      p <- pf_value_return_data() %>%
        ggplot(aes(x = date, y = performance)) +
        geom_line(color = "#337ab7") +
        scale_x_date(date_labels = "%m.%Y") +
        scale_y_continuous(labels = percent) +
        labs(x = "", y = "Performance") +
        theme_bw()
      
      ggplotly(p) %>% 
        config(displayModeBar = F)
      })
    
    output[["stats_gg_pf_dd"]] <- renderPlotly({
      req(pf_value_return_data())
      
      p <- pf_value_return_data() %>% 
          mutate(drawdown = calc_drawdown(ret)) %>% 
          ggplot(aes(x = date, y = drawdown)) +
          geom_line(color = "red") +
          scale_x_date(date_labels = "%m.%Y") +
          scale_y_continuous(labels = percent) +
          labs(x = "", y = "Drawdown") +
          theme_bw()
      
      ggplotly(p) %>% 
        config(displayModeBar = F)
      })
    
    list(
      column(2,
             tags$b("Selected portfolio:"),
             uiOutput("constitutes")),
      column(6,
             plotlyOutput("stats_gg_pf", height = "300px"),# %>% withSpinner(type = 1, proxy.height = "300px"),
             plotlyOutput("stats_gg_pf_dd", height = "200px")))
  })
  

  ## 2.6. correlations ----
  output$correlations2 <- renderUI({
    req(table_stats())
    req(table_stats_pf())
    
    output[["stats_tbl_pf"]] <- renderTable({
      req(table_stats_pf())
      
      table_stats_pf() %>% 
        select(-symbol, -ends_with("_num"))
    })
    
    output[["returnsdplot"]] <- renderPlotly({
      req(table_stats())
      req(table_stats_pf())
      
      p <- bind_rows(table_stats(), table_stats_pf()) %>% 
        ggplot(aes(x = volatility_num, y = return_num, color = symbol)) +
        geom_point(size = 2, alpha = 0.4) +
        geom_text(aes(label = symbol), nudge_x = 0.01) +
        scale_y_continuous(labels = percent) +
        scale_x_continuous(labels = percent) +
        labs(x = "Volatility", y = "Return") +
        theme_bw() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% 
        config(displayModeBar = F)
    })
    
    column(4,
           # plotOutput("corplot", height = "300px", width = "300px"),
           tags$b("Portfolio"),
           tableOutput("stats_tbl_pf"),
           hr(),
           plotlyOutput("returnsdplot", height = "200px"))
  })
  
}

shinyApp(ui, server)
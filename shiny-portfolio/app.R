# 0. setup ----
source("setup.R")
# source("data.R")

# 1. UI ----
ui <- fluidPage(
  tags$head(tags$style('.btn-group{ margin-top: 5px;}')),
  # fluidRow(div(style = "margin-top:10px")),
  div(style = "margin-top:10px"),
  fluidRow(
    column(2,
           sliderInput("daterange", "Calculation period (from-to)",
                       min = Sys.Date() - years(30),
                       max = Sys.Date(),
                       value = c(Sys.Date() - years(10),
                                 Sys.Date()),
                       timeFormat="%Y-%m-%d")),
    uiOutput("sliders")
  ),
  fluidRow(
    column(2,
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
           pickerInput(
             inputId = "etfs",
             label = "", 
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
           actionLink("reset", "reset selection"),
           div(style = "margin-top:10px"),
           radioGroupButtons("rebalance_on",
                             "rebalance on:",
                             choices = c("days", "weeks", "months", "quarters", "years"
                                         # , "never", "when it deviates by more than ..."
                                         ),
                             selected = "months",
                             individual = T,
                             status = "primary"),
           uiOutput("rebalance_deviation")),
    uiOutput("etf_stats")
  ),
  hr(),
  fluidRow(
    uiOutput("pf_stats"),
    uiOutput("correlations")
    # tableOutput(paste0("stats_tbl_", "SPY"))
    
  )
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
  
  output$rebalance_deviation <- renderUI({
    req(input$rebalance_on)
    
    if (input$rebalance_on == "when it deviates by more than ...") {
      numericInput("repalance_percent", label = "deviation in %-points", min = 0, max = 20, value = 5, step = 1, width = "30%")
    }
  })
  
  ## 2.4. etf stats ----
  price_return_data <- reactive({
    req(input$etfs)
    
    get_yahoo_prices_cache(symbol = input$etfs, from = input$daterange[1], to = input$daterange[2]) %>% 
      group_by(symbol) %>% 
      mutate(ret = adjusted/lag(adjusted) - 1) %>% 
      ungroup()
  })
  
  observe({
    mindate <- price_return_data() %>% 
      group_by(symbol) %>% 
      summarise(mindate = min(date)) %>% 
      summarise(max(mindate)) %>% 
      pull()
    
    updateSliderInput(session, "daterange", value = c(mindate, input$daterange[2]))
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
             
             output[[paste0("stats_gg_", x)]] <- renderPlot({
               price_return_data() %>% 
                 filter(symbol == x) %>% 
                 ggplot(aes(x = date, y = adjusted)) +
                 geom_line(color = "red") +
                 theme_bw()
             })
             
             column(2,
                    tableOutput(paste0("stats_tbl_", x)),
                    plotOutput(paste0("stats_gg_", x), height = "150px")
                    ) 
             }
           )
  })
  
  ## 2.5. portfolio ----
  pf_weights <- reactive({
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
      print(pf_weights())
      price_return_data() %>% 
        tq_portfolio(assets_col = symbol, returns_col = ret, weights = pf_weights(), rebalance_on = input$rebalance_on, 
                     col_rename = "ret") %>% 
        mutate(performance = cumprod(1+ret)-1)
    }
  })
  
  table_stats_pf <- reactive({
    pf_value_return_data() %>%
      summarise(return_num = prod(1 + ret, na.rm = T)^(1/(n()/252)) - 1,
                volatility_num = sd(ret, na.rm = T)*sqrt(250),
                SR_num = return_num/volatility_num) %>%
      mutate(return = percent(return_num),
             volatility = percent(volatility_num),
             SR = number(SR_num, accuracy = 0.01),
             symbol = "_Portfolio")
  }) 

  output$pf_stats <- renderUI({
    req(pf_value_return_data())

    output[["stats_tbl_pf"]] <- renderTable({
      table_stats_pf() %>% 
        select(-symbol, -ends_with("_num"))
    })
    
    output[["stats_gg_pf"]] <- renderPlot({
      pf_value_return_data() %>%
        ggplot(aes(x = date, y = performance)) +
        geom_line(color = "#337ab7") +
        scale_x_date(date_labels = "%m.%Y") +
        scale_y_continuous(labels = percent) +
        labs(x = "", y = "drawdown") +
        theme_bw()
      })
    
    output[["stats_gg_pf_dd"]] <- renderPlot({
      pf_value_return_data() %>% 
          mutate(drawdown = calc_drawdown(ret)) %>% 
          ggplot(aes(x = date, y = drawdown)) +
          geom_line(color = "red") +
          scale_x_date(date_labels = "%m.%Y") +
          scale_y_continuous(labels = percent) +
          labs(x = "", y = "drawdown") +
          theme_bw()
      })
    
    list(
      column(2,
             tableOutput("stats_tbl_pf")),
      column(4,
             plotOutput("stats_gg_pf", height = "300px"),
             plotOutput("stats_gg_pf_dd", height = "200px")))
  })
  

  ## 2.6. correlations ----
  output$correlations <- renderUI({
    corr <- price_return_data() %>%
      select(symbol, date, ret) %>% 
      filter(!is.na(ret)) %>% 
      pivot_wider(names_from = "symbol", values_from = "ret") %>% 
      select(-date) %>%
      cor()
    
    output[["corplot"]] <- renderPlot({
      ggcorrplot(corr, hc.order = T, type = "lower",
               lab = TRUE, 
               ggtheme = ggplot2::theme_bw())
    })
    
    output[["returnsdplot"]] <- renderPlot({
      bind_rows(table_stats(), table_stats_pf()) %>% 
        ggplot(aes(x = volatility_num, y = return_num, color = symbol)) +
        geom_point() +
        theme_bw()
    })
    
    column(4,
           plotOutput("corplot", height = "300px"),
           plotOutput("returnsdplot", height = "200px"))
  })
  
}

shinyApp(ui, server)
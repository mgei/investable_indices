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
           actionGroupButtons(
             inputIds = c("reset_picker", "sample1_picker", "sample2_picker"),
             labels = list("reset", "SPY, BND, GLD", "VT, BND"),
             status = "primary"),
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
           radioGroupButtons("rebalance_on",
                             "rebalance frequency:",
                             choices = c("daily", "weekly", "monthly", "quarterly", "annually", "never",
                                         "when it deviates by more than ..."),
                             selected = "monthly",
                             individual = T,
                             status = "primary"),
           uiOutput("rebalance_deviation")),
    tableOutput("etf_stats")
  ),
  hr(),
  fluidRow(
    
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
  
  observeEvent(input$sample1_picker, {
    updatePickerInput(session, "etfs",
                      selected = c("SPY", "GLD", "BND"))
  })
  
  observeEvent(input$sample2_picker, {
    updatePickerInput(session, "etfs",
                      selected = c("VT", "BND"))
  })
  
  ## 2.2. slider inputs ----
  
  observeEvent(input$etfs, {
    # subitems(paste0("slider_", input$etfs))
    subitems(input$etfs)
  })
  
  output$sliders <- renderUI({
    req(input$etfs)
    
    lapply(subitems(), 
           function(x) { column(2, sliderInput(x, str_remove(x, "slider_"), value = 1/length(subitems()), min = 0, max = 1)) })
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
            updateSliderInput(session, x, value = 1 - (sum_of_sliders() - input[[paste0("slider_", x)]]))
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
  
  output$etf_stats <- renderUI({
    req(input$etfs)
    
    lapply(subitems(), 
           function(x) {
             output[[paste0("stats_tbl_", x)]] <- renderTable({
               price_return_data() %>% 
                 filter(symbol == x) %>% 
                 summarise(return = prod(1 + ret, na.rm = T)^(1/(n()/252)) - 1,
                           volatility = sd(ret, na.rm = T)*sqrt(250),
                           SR = return/volatility) %>% 
                 mutate(return = percent(return),
                        volatility = percent(volatility),
                        SR = number(SR, accuracy = 0.01))
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
                    ) })
  })
  
  ## 2.5. portfolio stats ----
  pf_value_return_data <- reactive({
    
  }) 
  
}

shinyApp(ui, server)
# 0. setup ----
source("setup.R")
# source("data.R")

# 1. UI ----
ui <- fluidPage(
  tags$head(tags$style('.btn-group{ margin-top: 5px;}')),
  fluidRow(tags$br()),
  fluidRow(
    column(2,
           actionGroupButtons(
             inputIds = c("reset_picker", "sample1_picker", "sample2_picker"),
             labels = list("reset", "SPY, BND, GLD", "VT, BND"),
             status = "primary"),
           pickerInput(
             inputId = "etfs",
             label = "", 
             choices = NULL,
             multiple = T,
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
               "max-options" = 3,
               "max-options-text" = "select no more than 3!"
             ))),
    uiOutput("sliders")
  ),
  fluidRow(
    column(2,
           radioGroupButtons("rebalance_on",
                             "rebalance frequency:",
                             choices = c("daily", "weekly", "monthly", "quarterly", "annually", "never",
                                         "when it deviates by more than ..."),
                             selected = "monthly",
                             individual = T,
                             status = "primary"),
           uiOutput("rebalance_deviation")),
    uiOutput("etf_stats")
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
    subitems(paste0("slider_", input$etfs))
  })
  
  output$sliders <- renderUI({
    req(input$etfs)
    
    lapply(subitems(), 
           function(x) { column(2, sliderInput(x, str_remove(x, "slider_"), value = 1/length(subitems()), min = 0, max = 1)) })
  })
  
  sum_of_sliders <- reactive({
    req(input$etfs)
    
    sum <- 0
    for(s in subitems()) {
      sum <- sum + input[[s]]
    }
    sum
  })
  
  observe({
    lapply(
      subitems(),
      function(x) {
        observeEvent(input[[x]], {
          req(sum_of_sliders())
          
          if (sum_of_sliders() > 1) {
            updateSliderInput(session, x, value = 1 - (sum_of_sliders() - input[[x]]))
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
  output$etf_stats <- renderUI({
    req(input$etfs)
    
    lapply(subitems(), 
           function(x) { column(2, x) })
  })
  
}

shinyApp(ui, server)
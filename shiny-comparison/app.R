source("setup.R")

ui <- fluidPage(
  fluidRow(
    column(4,
           pickerInput(inputId = "stock_picker", label = "previous", choices = c("", str_remove(list.files("data/cache/"), ".RDS")))),
    column(4,
           textInput(inputId = "stock_search", label = "search", value = "")),
    column(4, "")
  ),
  fluidRow(
    column(3, 
           textOutput("picked"),
           textOutput("searched")),
    column(6,
           tabsetPanel(type = "tabs",
                       tabPanel("Plot", plotlyOutput("performanceplot")),
                       tabPanel("Summary", "sum"),
                       tabPanel("Table", "tbl")
           )),
    column(3, "")
    )
)

server <- function(input, output, session) {

  output$picked <- renderText({
    input$stock_picker
  })
  
  output$searched <- renderText({
    input$stock_search
  })

  
    
  stock_picked <- reactive({
    req(input$stock_picker)
    
    if (input$stock_picker != "") {
      get_prices(symbol = input$stock_picker)
    }
  })
  
  stock_searched <- reactive({
    req(input$stock_search)

    if (input$stock_search != "") {
      print("yes")
      get_prices(symbol = input$stock_search)
    }
  })
  
  stock <- reactive({
    print(stock_searched())
    
    if (!is.null(stock_searched())) {
      stock_searched()
    } else {
      stock_picked()
    }
  })
  
  output$performanceplot <- renderPlotly({
    # validate(need(stock()))
    
    pltly <- stock() %>% 
      dplyr::group_by(symbol) %>% 
      dplyr::mutate(adjusted = adjusted / adjusted[1L]) %>% 
      plotly::plot_ly(x = ~date, y = ~adjusted, color = ~symbol,
                      type = "scatter", mode = "lines") %>% 
      plotly::layout(dragmode = "zoom", 
                     datarevision = 0) %>% 
      rangeslider()
    

    
    htmlwidgets::onRender(pltly, onRenderRebaseTxt)
  })
}

shinyApp(ui, server)
library(shiny)
library(DT)

data <- mtcars %>% 
  rownames_to_column() %>% 
  as_tibble()

ui <- fluidPage(
  DTOutput("mytable")
)

server <- function(input, output, session) {
  
  row_selected <- NULL
  
  # Here you read the URL parameter from session$clientData$url_search
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['rownumber']])) {
      row_selected <<- query[['rownumber']] %>% as.integer()
      page_selected <<- row_selected
    }
  })

  
  output$mytable <- renderDT({
    datatable(data,
              selection = list(mode = "single", target = "row", selected = row_selected),
              options = list(pageLength = 10, info = FALSE, displayStart = max(row_selected-10, 0))) #max(ceiling(row_selected/10), 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
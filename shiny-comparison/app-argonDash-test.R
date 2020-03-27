library(shiny)
library(argonDash)
library(DT)

ui <- argonDashPage(
    title = "Porfact",
    description = "Personal Trading Portfolio Monitoring Dashboard",
    author = "Shawn Lin",
    # header = argonDash::argonDashHeader(
    #   gradient = T,
    #   color = 'info',
    #   h4('Porfact Portfolio Monitor', style = 'color:white;text-align:center;font-size:2em;')
    # ),
    body = argonDashBody(
      argonRow(
        argonColumn("reload", width = 2),
        argonColumn(DTOutput("listtable"), width = 6),
        argonColumn("selected", width = 4)
      ),
      argonRow(
        argonColumn("options", width = 2),
        argonColumn("plot", width = 6),
        argonColumn("stats", width = 4)
      ),
      argonRow(
        argonColumn("table performance", width = 12)
      ),
      
    )
)

server <- function(input, output, session) {
  output$listtable <- renderDT({
    datatable(mtcars)
  })
}

shinyApp(ui, server)
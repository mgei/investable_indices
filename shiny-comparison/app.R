source("setup.R")

ui <- fluidPage(
  # textInput("symbol"),
  plotlyOutput("performanceplot")
  
)

server <- function(input, output, session) {
  # output$text <- renderText({
  #   input$symbol
  # })
  
  output$performanceplot <- renderPlotly({
    pltly <- stocks %>% 
      dplyr::group_by(symbol) %>% 
      dplyr::mutate(adjusted = adjusted / adjusted[1L]) %>% 
      plotly::plot_ly(x = ~date, y = ~adjusted, color = ~symbol,
                      type = "scatter", mode = "lines") %>% 
      plotly::layout(dragmode = "zoom", 
                     datarevision = 0) %>% 
      rangeslider()
    
    onRenderRebaseTxt <- "
    function(el, x) {
      el.on('plotly_relayout', function(rlyt) {
        var nrTrcs = el.data.length;
        // array of x index to rebase to; defaults to zero when all x are shown, needs to be one per trace
        baseX = Array.from({length: nrTrcs}, (v, i) => 0);
        // if x zoomed, increase baseX until first x point larger than x-range start
        if (el.layout.xaxis.autorange == false) {
            for (var trc = 0; trc < nrTrcs; trc++) {
                while (el.data[[trc]].x[baseX[trc]] < el.layout.xaxis.range[0]) {baseX[trc]++;}
            }   
        }
        // rebase each trace
        for (var trc = 0; trc < nrTrcs; trc++) {
            el.data[trc].y = el.data[[trc]].y.map(x => x / el.data[[trc]].y[baseX[trc]]);
        }
        el.layout.yaxis.autorange = true; // to show all traces if y was zoomed as well
        el.layout.datarevision++; // needs to change for react method to show data changes
        Plotly.react(el, el.data, el.layout);
      });
    }
    "
    
    htmlwidgets::onRender(pltly, onRenderRebaseTxt)
  })
}

shinyApp(ui, server)
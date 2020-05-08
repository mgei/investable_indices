source("setup.R")
# source("data.R")

ui <- fluidPage(
  sliderInput("slider1", "slider1", value = 0.5, min = 0, max = 1),
  sliderInput("slider2", "slider2", value = 0.5, min = 0, max = 1),
  # selectizeInput("foo", "foo", choices = NULL, multiple = T),
  pickerInput(
    inputId = "Id084",
    label = "Portfolio constituents", 
    choices = NULL,
    multiple = T,
    options = list(
      `live-search` = TRUE,
      "none-selected-text" = "select your ETFs",
      # `actions-box` = TRUE,
      "max-options" = 3,
      "max-options-text" = "select no more than 3!"
    ))
  
)

server <- function(input, output, session) {
  # updateSelectizeInput(session, 'foo', choices = get_IB_etflist_cache("ARCA")$symbol, server = TRUE)
  
  updatePickerInput(session, "Id084", 
                    choices = get_IB_etflist_cache("ARCA")$symbol,
                    choicesOpt = list(
                      subtext = get_IB_etflist_cache("ARCA")$description))
  
  # multiInput()
}

shinyApp(ui, server)

# 
# pickerInput(
#   inputId = "Id093",
#   label = "Subtext", 
#   choices = rownames(mtcars),
#   choicesOpt = list(
#     subtext = paste("mpg", 
#                     mtcars$mpg,
#                     sep = ": "))
# )
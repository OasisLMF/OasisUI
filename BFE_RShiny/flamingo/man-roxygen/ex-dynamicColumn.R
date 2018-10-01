if (interactive()) {
  library(shiny)
  twoDynamicColumnsUI <- function(sidebar_w) {
    # make the UI rendering slow
    Sys.sleep(1)
    fluidRow(
      dynamicColumnUI(
        id = "sidebar", width = sidebar_w,
        radioButtons("radio", "Does radio selection persist re-sizing?", LETTERS[1:3])
      ),
      dynamicColumnUI(
        id = "main", width = 12L - sidebar_w,
        textInput("text", "Does text input persist re-sizing?", width = "100%")
      )
    )
  }
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Resizable columns with `dynamicColumn` module"),
    fluidPage(
      fluidRow(sliderInput("sidebar_w", "Sidebar Width", 1L, 11L, 3L, 1L)),
      fluidRow(checkboxInput("dynamic", "Use dynamicColumn", FALSE)),
      fluidRow(uiOutput("twocols"))
    )
  )
  server <- function(input, output) {
    observe(
      if (input$dynamic) {
        # UI rendering non-reactive to input$sidebar_w
        output$twocols <-
          renderUI(twoDynamicColumnsUI(isolate(input$sidebar_w)))
        # reactivity via dynamicColumn module
        callModule(dynamicColumn, "sidebar", reactive(input$sidebar_w))
        callModule(dynamicColumn, "main", reactive(12L - input$sidebar_w))
      } else {
        # UI rendering reactive to input$sidebar_w
        output$twocols <-
          renderUI(twoDynamicColumnsUI(input$sidebar_w))
      }
    )
  }
  shinyApp(ui, server)
}

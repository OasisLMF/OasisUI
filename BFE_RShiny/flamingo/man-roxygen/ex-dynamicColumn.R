if (interactive()) {
  library(shiny)
  twoFlexColumnsUI <- function(sidebar_w) {
    # make the UI rendering slow
    Sys.sleep(1)
    fluidRow(
      flexColumnUI(
        id = "sidebar", width = sidebar_w,
        radioButtons("radio", "Does radio selection persist re-sizing?", LETTERS[1:3])
      ),
      flexColumnUI(
        id = "main", width = 12L - sidebar_w,
        textInput("text", "Does text input persist re-sizing?", width = "100%")
      )
    )
  }
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Resizable columns with `flexColumn` module"),
    fluidPage(
      fluidRow(sliderInput("sidebar_w", "Sidebar Width", 1L, 11L, 3L, 1L)),
      fluidRow(checkboxInput("flex", "Use flexColumn", FALSE)),
      fluidRow(uiOutput("twocols"))
    )
  )
  server <- function(input, output) {
    observe(
      if (input$flex) {
        # UI rendering non-reactive to input$sidebar_w
        output$twocols <-
          renderUI(twoFlexColumnsUI(isolate(input$sidebar_w)))
        # reactivity via flexColumn module
        callModule(flexColumn, "sidebar", reactive(input$sidebar_w))
        callModule(flexColumn, "main", reactive(12L - input$sidebar_w))
      } else {
        # UI rendering reactive to input$sidebar_w
        output$twocols <-
          renderUI(twoFlexColumnsUI(input$sidebar_w))
      }
    )
  }
  shinyApp(ui, server)
}

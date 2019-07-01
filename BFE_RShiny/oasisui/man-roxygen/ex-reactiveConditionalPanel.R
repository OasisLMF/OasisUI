if (interactive()) {
  library(shiny)
  panels <- LETTERS[1:10]
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Reactive panel visibility `conditionalPanel`"),
    sidebarLayout(
      sidebarPanel(sliderInput(
        "active_nr", "Number of active panels",
        min = 0, max = length(panels), step = 1, round = TRUE, value = 0
      )),
      mainPanel(mapply(
        reactiveConditionalPanelUI,
        panels,  lapply(panels, div),
        SIMPLIFY = FALSE
      ))
    )
  )
  server <- function(input, output) {
    lapply(panels, function(x) {
      callModule(
        reactiveConditionalPanel, x,
        reactive(x %in% head(panels, input$active_nr))
      )
    })
  }
  shinyApp(ui = ui, server = server)
}

if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Reactive panel visibility via `conditionalPanels`"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "main_visible",
          "Active main panel",
          LETTERS[1:10]
        )
      ),
      mainPanel(
        conditionalPanelsUI(
          "main",
          c(
            mapply(SIMPLIFY = FALSE, textInput,
                   LETTERS[1:5], paste("Main Panel", LETTERS[1:5])),
            list(A = textInput("A-nother", "A-nother Main Panel"))
          )
        )
      )
    )
  )
  server <- function(input, output) {
    callModule(conditionalPanels, "main", reactive(input$main_visible))
  }
  shinyApp(ui = ui, server = server)
}

if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Multiple reactive panel visibility via `reactiveConditionalPanels`"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "main_visible",
          "Active main panel",
          LETTERS[1:10]
        )
      ),
      mainPanel(
        reactiveConditionalPanelsUI(
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
    callModule(reactiveConditionalPanels, "main", reactive(input$main_visible))
  }
  shinyApp(ui = ui, server = server)
}

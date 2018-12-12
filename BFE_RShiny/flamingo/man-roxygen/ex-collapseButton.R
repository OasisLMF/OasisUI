
# to become man-roxygen/ex-collapseButton.R
collapseButton(id = "abutton", id_collapse = "acollapse")
collapseButton(id = "abutton", id_collapse = "acollapse", collapsed = TRUE)

if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    tags$style(HTML('
      .collapsebtn:after {
      content:"-";
      float: right;
      }
      .collapsebtn.collapsed:after {
      content: "+";
      }
      '
    )),
    titlePanel("`collapseButton` example"),
    verticalLayout(
      div(collapseButton(
        id = "shelovesyou", id_collapse = "yeah", collapsed = TRUE,
        "She loves you..."
      )),
      bsplus::bs_collapse(id = "yeah", "...Yeah yeah yeah", show = FALSE),
      div(collapseButton(
        id = "shelovesme", id_collapse = "no", collapsed = FALSE,
        "She loves me..."
      )),
      bsplus::bs_collapse(id = "no", "...No no no", show = TRUE)
    )
  )
  server <- function(input, output) {}
  shinyApp(ui, server)
}

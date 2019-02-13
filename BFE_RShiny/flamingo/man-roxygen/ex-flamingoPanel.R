if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    # replace eventually with flamingo-tweaks.css via system.file()
    tags$style(HTML('
                    .collapsebtn:after {
                    font-family: "Font Awesome 5 Free"; font-weight: 900; content: "\\f068";
                    float: right;
                    }
                    .collapsebtn.collapsed:after {
                    content: "\\f31e";
                    }
                    '
    )),
    titlePanel("`flamingoPanel` examples"),
    flamingoPanel(
      "apanel",
      "...Yeah yeah yeah",
      heading = h4("She loves you...")
    ),
    flamingoPanel(
      "bpanel",
      "...Yeah yeah yeah",
      heading = h4("She loves you..."),
      collapsible = TRUE
    ),
    flamingoPanel(
      "cpanel",
      "...Yeah yeah yeah",
      heading = tagList(
        "She loves you...",
        actionButton("aa", icon("times"), style = "float: right")
      ),
      footer = fluidRow(column(
        12,
        "footer"
      )),
      collapsible = TRUE,
      show = FALSE
    )
  )
  server <- function(input, output) {}
  shinyApp(ui, server)
}

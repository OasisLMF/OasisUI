if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    # replace eventually with oasisui-tweaks.css via system.file()
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
    titlePanel("`oasisuiPanel` examples"),
    oasisuiPanel(
      "apanel",
      "...Yeah yeah yeah",
      heading = h4("She loves you...")
    ),
    oasisuiPanel(
      "bpanel",
      "...Yeah yeah yeah",
      heading = h4("She loves you..."),
      collapsible = TRUE
    ),
    oasisuiPanel(
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


# adapted from shinyWidgets::panel() including IDs and collapsible (inspired by
# shinyBS::bsCollapsePanel())
# TODO: roxygen tags including example (see below)
flamingoPanel <- function(id, ..., heading = NULL, footer = NULL, status = "default", collapsible = FALSE, show = TRUE) {

  with_id <- function(x) paste(id, x, sep = "-")
  status <- match.arg(
    arg = status,
    choices = c("default", "primary", "success", "info", "warning", "danger")
  )
  htmltools::tags$div(
    id = id,
    class = paste0("panel panel-", status),
    if (!is.null(heading) || collapsible) {
      htmltools::tags$div(
        id = with_id("heading"), class = "panel-heading",
        fluidRow(column(
          12,
          if (is.character(heading)) {
            # TODO: fine-tune font size
            htmltools::tags$div(heading, style = "font-size: 24px; display: inline-block")
          } else {
            heading
          },
          if (collapsible) {
            collapseButton(with_id("collapse-button"), with_id("body"), style = "display: inline-block; float: right", collapsed = !show)
          }
        ))
      )
    },
    shiny::tags$div(
      id = with_id("body"), class = paste("panel-collapse collapse", if (show) "in"),
      role = "tabpanel",
      shiny::tags$div(class = "panel-body", ...)
    ),
    if (!is.null(footer)) {
      htmltools::tags$div(
        id = with_id("footer"), class = "panel-footer", footer
      )
    }
  )
}

if (FALSE) {
  # to become man-roxygen/ex-flamingoPanel.R
  if (interactive()) {
    library(shiny)
    ui <- fluidPage(
      # replace eventually with flamingo-tweaks.css via system.file()
      tags$style(HTML('
      .collapsebtn:after {
      font-family: "FontAwesome"; font-weight: 900; content: "\\f068";
      float: right;
      }
      .collapsebtn.collapsed:after {
      content: "\\f065";
      }
      '
      )),
      titlePanel("`flamingoPanel` examples"),
      flamingoPanel(
        "apanel",
        "...Yeah yeah yeah",
        heading ="She loves you..."
      ),
      flamingoPanel(
        "bpanel",
        "...Yeah yeah yeah",
        heading ="She loves you...",
        collapsible = TRUE
      ),
      flamingoPanel(
        "cpanel",
        "...Yeah yeah yeah",
        heading = tagList(
          "She loves you...",
          shiny::actionButton("aa", icon("times"), style = "float: right")
        ),
        footer = fluidRow(column(
          12,
          shiny::actionButton("aa", "+", style = "float: left")
        )),
        collapsible = TRUE,
        show = FALSE
      ),
      flamingoPanel(
        "dpanel",
        footer = fluidRow(column(
          12,
          shiny::actionButton("aa", "+", style = "float: left")
        )),
        show = FALSE
      ),

      NULL
    )
    server <- function(input, output) {}
    shinyApp(ui, server)
  }
}


# TODO: roxygen tags including example (see below)
# shall we use flamingo::actionButton()?
collapseButton <- function(id, id_collapse, ..., width = NULL, collapsed = FALSE) {
  shiny::actionButton(id, NULL, NULL, ..., width = width) %>%
    bsplus::bs_attach_collapse(id_collapse)  %>%
    tagAppendAttributes(class = paste("collapsebtn", if (collapsed) "collapsed"))
}
if (FALSE) {
  # to become man-roxygen/ex-collapseButton.R
  collapseButton(id = "abutton", id_collapse = "acollapse")
  collapseButton(id = "abutton", id_collapse = "acollapse", collapsed = TRUE)
  if (interactive()) {
    library(shiny)
    ui <- fluidPage(
      # replace eventually with flamingo-tweaks.css via system.file()
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
        bsplus::bs_collapse(id = "yeah", "...Yeah yeah yeah", show = FALSE),
        div(collapseButton(
          id = "shelovesyou", id_collapse = "yeah", collapsed = TRUE,
          "She loves you..."
        ))
      )
    )
    server <- function(input, output) {}
    shinyApp(ui, server)
  }
}

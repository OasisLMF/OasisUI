#' flamingoPanel
#'
#' @rdname flamingoPanel
#'
#' @description Adapted from shinyWidgets::panel() including IDs and collapsible
#' (inspired by shinyBS::bsCollapsePanel()).
#'
#' @inheritParams accountDefinitionUI
#' @param ... Additional parameters.
#' @param heading NULL by default.
#' @param footer NULL by default.
#' @param status default.
#' @param show TRUE.
#' @param collapsible FALSE.
#'
#' @importFrom htmltools tags
#'
#' @export
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
          tagAppendAttributes(
            tagAppendChild(
              flamingoPanelHeading(heading),
              if (collapsible) {
                collapseButton(with_id("collapse-button"), with_id("body"), style = "float: right", collapsed = !show)
              }
            ))
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
          actionButton("aa", "+", style = "float: left")
        )),
        collapsible = TRUE,
        show = FALSE
      ),
      flamingoPanel(
        "dpanel",
        footer = fluidRow(column(
          12,
          actionButton("aa", "+", style = "float: left")
        )),
        show = FALSE
      ),

      NULL
    )
    server <- function(input, output) {}
    shinyApp(ui, server)
  }
}

#' collapseButton
#'
#' @rdname collapseButton
#'
#' @inheritParams accountDefinitionUI
#' @inheritParams accountDefinitionUI
#' @inheritParams dynamicColumn
#' @param ... Additional paramters.
#' @param collapsed FALSE.
#'
#' @return Collapsed button.
#'
#' @importFrom bsplus bs_attach_collapse
#' @importFrom htmltools tags
collapseButton <- function(id, id_collapse, ..., width = NULL, collapsed = FALSE) {
  actionButton(id, NULL, NULL, ..., width = width) %>%
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

#' flamingoPanelHeading
#'
#' @rdname flamingoPanelHeading
#'
#' @param heading
#'
#' @return smth.
flamingoPanelHeading <- function(heading) {
  if (is.character(heading)) {
    # TODO: fine-tune font size
    htmltools::tags$span(heading, style = "font-size: 22px;")
  } else {
    heading
  }
}

#' flamingoPanelHeadingOutput
#'
#' @rdname flamingoPanelHeadingOutput
#'
#' @param outputId Output Id.
#' @param ... Additional parameters.
#'
#' @return smth.
flamingoPanelHeadingOutput <- function(outputId, ...) {
  div(uiOutput(outputId, inline = TRUE, ...))
}

#' renderflamingoPanelHeading
#'
#' @rdname renderflamingoPanelHeading
#'
#' @param expr Expression.
#' @param env Environment.
#' @param ... Additional parameters.
#'
#' @return smth.
renderflamingoPanelHeading <- function(expr, env = parent.frame(), ...) {
  renderUI(call("flamingoPanelHeading", substitute(expr)), quoted = TRUE, env = env, ...)
}

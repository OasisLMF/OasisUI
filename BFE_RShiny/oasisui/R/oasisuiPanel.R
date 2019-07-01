#' Oasisui UI Panel
#'
#' Oasisui UI panel with collapsing capabilities.
#'
#' @param id String ID of the panel.
#' @param ... UI elements to include inside the panel.
#' @param heading Title for the panel.
#' @param footer Footer for the panel.
#' @param status Bootstrap status for contextual alternative.
#' @param collapsible Logical flag specifying if the panel is collapsible.
#' @param show Logical flag specifying if a collapsible panel should be
#'   initially shown as not collapsed.
#'
#' @details Adapted from [shinyWidgets::panel()] including IDs and collapsible
#' (inspired by `bsCollapsePanel` from the `shinyBS` package).
#'
#' @return The UI definition of the panel.
#'
#' @example man-roxygen/ex-oasisuiPanel.R
#'
#' @export
#'
#' @md
oasisuiPanel <- function(id, ..., heading = NULL, footer = NULL,
                          status = "default", collapsible = FALSE, show = TRUE) {

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
              oasisuiPanelHeading(heading),
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


#' Collapse Button
#'
#' Collapse button associated to a collapsible UI element.
#'
#' @param id ID of the button
#' @param id_collapse ID of the collapsible UI element.
#' @inheritParams shiny::actionButton
#' @param collapsed Initial state of the button.
#'
#' @example man-roxygen/ex-collapseButton.R
#'
#' @export
#'
#' @md
collapseButton <- function(id, id_collapse, ..., width = NULL, collapsed = FALSE) {
  actionButton(id, NULL, NULL, ..., width = width) %>%
    bsplus::bs_attach_collapse(id_collapse)  %>%
    tagAppendAttributes(class = paste("collapsebtn", if (collapsed) "collapsed"))
}


#' Panel Heading
#'
#' Construct an appropriate heading for a [oasisuiPanel].
#'
#' @param heading The desired heading, either a character string (which gets
#'   properly wrapped) or a tags list (returned as-is).
#' @inheritParams shiny::uiOutput
#'
#' @export
#'
#' @md
oasisuiPanelHeading <- function(heading) {
  if (is.character(heading)) {
    # TODO: fine-tune font size
    htmltools::tags$span(heading, style = "font-size: 22px;")
  } else {
    heading
  }
}

#' Panel Heading UI Output
#'
#' Panel heading UI placeholder to be rendered dynamically via
#' [renderOasisuiPanelHeading()].
#'
#' @param outputId ID of the dynamical panel heading.
#' @inheritParams shiny::uiOutput
#'
#' @seealso [renderOasisuiPanelHeading()]
#'
#' @export
#'
#' @md
oasisuiPanelHeadingOutput <- function(outputId, ...) {
  div(uiOutput(outputId, inline = TRUE, ...))
}

#' Render Heading UI
#'
#' Render reactive panel heading content dynamically based on server logic.
#'
#' @inheritParams shiny::renderUI
#' @param ... Further arguments passed to [shiny::renderUI()].
#'
#' @seealso [oasisuiPanelHeadingOutput()]
#'
#' @export
#'
#' @md
renderOasisuiPanelHeading <- function(expr, env = parent.frame(), ...) {
  renderUI(call("oasisuiPanelHeading", substitute(expr)), quoted = TRUE, env = env, ...)
}

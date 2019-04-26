#' flamingoIncrementalPanel
#'
#' @rdname flamingoIncrementalPanel
#'
#' @description Incremental flamingo panel module, including a button to add
#'   a new panel above.
#'
#' @template params-module
#' @param panels_state State (taken/available) of panels placeholders, a
#'   [reactiveValues()] object with a named scalar logical element for each
#'   panel ID. The state is updated upon addition / removal of panels.
#' @param new_content_IDs Named character vector of IDs to be used for the
#'   content of each panel upon its creation. Names match the elements of
#'   `panels_state`.
#' @param new_content_fun Function used to populate any new panel.
#' @param ...  For the module server function, additional aruments passed to
#'   `new_content_fun`. For the module UI function, the elements to include
#'   inside the panel.
#' @param new_headings Heading content to be used for each panel upon its
#'   creation, as a named character vector or list. Names match the elements of
#'   `panels_state`.
#'
#' @example man-roxygen/ex-incrementalPanels.R
#'
#' @importFrom utils head
#'
#' @export
#'
#' @md
flamingoIncrementalPanel <- function(input, output, session, panels_state,
                                     new_content_IDs, new_content_fun, ..., new_headings = NULL,
                                     collapsible = FALSE, show = TRUE) {
  id <- session$ns(NULL)
  observeEvent(input$add, {
    taken <- unlist(reactiveValuesToList(panels_state))
    new_i <- head(which(!taken), 1L)
    new_id <- names(taken)[new_i]
    cat("add", new_id, "\n")
    if (length(new_id) > 0) {
      panels_state[[new_id]] <- TRUE
      new_content_id <- new_content_IDs[new_id]
      insertUI(
        immediate = TRUE,
        sprintf("#%s", id), "beforeBegin",
        flamingoIncrementalPanelUI(new_id, new_content_fun(new_content_id, ...), heading = new_headings[[new_id]],
                                   collapsible = collapsible, show = show)
      )
    } else {
      flamingoNotification(type = "warning",
                           "Maximum number of panels reached.")
    }
  })
  observeEvent(input$delete, {
    showModal(modalDialog(
      title = "Remove the selected panel?",
      "Do you want to remove the selected panel?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("del_ok"), "OK")
      )
    ))
  })
  observeEvent(input$del_ok, {
    removeModal()
    cat("delete", id, "\n")
    removeUI(sprintf("#%s", id), immediate = TRUE)
    panels_state[[id]] <- FALSE
  })

}


#' @rdname flamingoIncrementalPanel
#'
#' @inheritParams flamingoPanel
#' @param removable Logical flag specifying if the panel can be removed.
#'
#' @export
#'
#' @md
flamingoIncrementalPanelUI <- function(id, ..., heading = NULL, footer = NULL, status = "default",
                                       collapsible = FALSE, show = TRUE, removable = TRUE) {
  ns <- NS(id)
  flamingoPanel(
    id = id,
    ...,
    heading = tagAppendChildren(
      flamingoPanelHeading(heading),
      actionButton(ns("add"), icon("plus"), style = "float: left; margin-right: 12px"),
      if (removable) actionButton(ns("delete"), icon("times"), style = "float: right")
    ),
    footer = footer,
    status = status,
    collapsible = collapsible,
    show = show
  )
}


# Utility for constructing a named reactiveVal for the given IDs
panelsState <- function(IDs) {
  state <- reactiveValues()
  for (id in IDs) {
    state[[id]] <- FALSE
  }
  state
}


#' callIncrementalPanelModules
#'
#' This is a convenience wrapper to enable the server logic of a number of
#' [flamingoIncrementalPanel]s, returning their reactive state and a function to
#' remove them all.
#'
#' @param IDs Character vector of IDs to be used for the content of
#'   each panel upon its creation.
#' @param ID_0 Character string with the ID of an existing initial panel.
#' @param contentIDs Character vector of IDs to be used for the content of
#'   each panel upon its creation.
#' @param contentUI Function used to populate any new panel.
#' @param headings Heading content to be used for each panel upon its
#'   creation, as a character vector or list.
#' @param ns Namespace function, typically obtained via [shiny::NS()].
#' @inheritParams flamingoIncrementalPanel
#'
#' @return
#' A `list` with components:
#' * `$state`: The reactive state of the panels (see
#'     [flamingoIncrementalPanel()]).
#' * `$remove_all`: A Function to remove all panels.
#'
#' @example man-roxygen/ex-incrementalPanels.R
#'
#' @export
#'
#' @md
callIncrementalPanelModules <- function(IDs, ID_0,
                                        contentIDs, contentUI,
                                        headings = NULL,
                                        collapsible = FALSE, show = TRUE,
                                        ns = identity) {
  panels_state <- panelsState(ns(IDs))
  # assign IDs as names, since positional matching should not be used with
  # reactiveValues state
  contentIDs <- stats::setNames(ns(contentIDs), ns(IDs))
  if (!is.null(headings)) {
    headings <- stats::setNames(headings, ns(IDs))
  }
  # panels modules
  lapply(
    c(IDs, ID_0),
    callModule, module = flamingoIncrementalPanel,
    panels_state,
    contentIDs, contentUI,
    new_headings = headings,
    collapsible = collapsible, show = show
  )

  list(
    state = panels_state,
    remove_all = function() {
      state_vec <- unlist(reactiveValuesToList(panels_state))
      IDs <- names(state_vec)[state_vec]
      cat("delete", IDs, "\n")
      lapply(IDs, function(id) {
        removeUI(sprintf("#%s", id), immediate = TRUE)
        panels_state[[id]] <- FALSE
      })
      invisible(IDs)
    }
  )
}

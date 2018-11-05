#' flamingoIncrementalPanelUI
#'
#' @rdname flamingoIncrementalPanelUI
#'
#' @param removable TRUE.
#' @inheritParams flamingoPanel
#'
#' @importFrom htmltools tags
#'
#' @export
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

#' flamingoIncrementalPanel
#'
#' @rdname flamingoIncrementalPanel
#'
#' @inheritParams flamingoModule
#' @param new_content_IDs ID with new content.
#' @param new_content_fun Function with new content.
#' @param new_headings New heading.
#' @param panels_state State of panel.
#'
#' @return smth.
#'
#' @importFrom htmltools tags
#' @importFrom utils head
#'
#' @export
flamingoIncrementalPanel <- function(input, output, session, panels_state,
                                     new_content_IDs, new_content_fun, ..., new_headings = NULL,
                                     collapsible = FALSE, show = TRUE) {
  id <- session$ns(NULL)
  observeEvent(input$add, {
    taken <- panels_state()
    new_i <- head(which(!taken), 1L)
    new_id <- names(taken)[new_i]
    cat("add", new_id, "\n")
    if (length(new_id) > 0) {
      taken[new_id] <- TRUE
      new_content_id <- new_content_IDs[new_i]
      insertUI(
        immediate = TRUE,
        sprintf("#%s", id), "beforeBegin",
        flamingoIncrementalPanelUI(new_id, new_content_fun(new_content_id, ...), heading = new_headings[[new_i]],
                                   collapsible = collapsible, show = show)
      )
      panels_state(taken)
    } else {
      flamingoNotification("Reached maximum number of panels", type = "warning")
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
    taken <- panels_state()
    taken[id] <- FALSE
    panels_state(taken)
  })
}

#' panelsState
#'
#' @rdname panelsState
#'
#' @param IDs IDs.
#'
#' @return smth.
#'
#' @export
panelsState <- function(IDs) {
  reactiveVal(
    setNames(rep(FALSE, length(IDs)), IDs)
  )
}

#' callIncrementalPanelModules
#'
#' @rdname callIncrementalPanelModules
#'
#' @inheritParams flamingoPanel
#' @param IDs IDs.
#' @param ID_0 ID_0.
#' @param contentIDs content of the IDs.
#' @param contentUI content of the UI.
#'
#' @return smth.
#'
#' @export
callIncrementalPanelModules <- function(IDs, ID_0,
                                        contentIDs, contentUI, ...,
                                        headings = NULL,
                                        collapsible = FALSE, show = TRUE,
                                        ns = identity) {
  panels_state <- panelsState(ns(IDs))
  # panels modules
  lapply(
    c(IDs, ID_0),
    callModule, module = flamingoIncrementalPanel,
    panels_state,
    ns(contentIDs), contentUI,
    new_headings = headings,
    collapsible = collapsible, show = show
  )

  list(
    state = panels_state,
    remove_all = function() {
      IDs <- names(panels_state()[panels_state()])
      cat("delete", IDs, "\n")
      lapply(IDs, function(id) {
        removeUI(sprintf("#%s", id), immediate = TRUE)
      })
      panels_state(panels_state() & FALSE)
      invisible(IDs)
    }
  )
}

if (FALSE) {
  if (interactive()) {
    library(shiny)
    n_panels <- 10L
    # Example module
    examplePanelUI <- function(id) {
      ns <- NS(id)
      verticalLayout(
        textInput(ns("txt_in"), label = paste("type something", id)),
        textOutput(ns("txt_out")),
        actionButton(ns("upd"), "Update")
      )
    }
    examplePanel <- function(input, output, session, reset = reactive(FALSE)) {

      if (FALSE) {
        txt <- eventReactive(input$upd, {
          input$txt_in
        })
        output$txt_out <- renderText(txt())
        txt
      } else {
        txt <- reactiveVal(NULL)
        observe({
          reset()
          txt(NULL)
        })
        observeEvent(input$upd, txt(input$txt_in))
        output$txt_out <- renderText(txt())
        reactive(txt())
      }
    }
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
      ')),
      titlePanel("Dynamic panels"),
      verticalLayout(
        actionButton("delete_all", "Remove all panels"),
        flamingoIncrementalPanelUI(
          "start-panel", heading = "Add a new panel",
          collapsible = FALSE, show = FALSE, removable = FALSE
        )
      )
    )
    server <- function(input, output, session) {
      # NOTE that, since we are using server logic to create UI elements, the IDs
      # used for the UI components must include session$ns (relevant for module
      # server functions)
      ns <- session$ns
      panel_IDs <- paste0("extpanel-", c(seq_len(n_panels)))
      # content IDs used for the content module server and UI
      # content modules
      content_IDs <- paste0("content-", seq_len(n_panels))
      all_panels <- callIncrementalPanelModules(
        panel_IDs, "start-panel", content_IDs,
        examplePanelUI,
        headings = lapply(seq_len(n_panels), function(i) {flamingoPanelHeadingOutput(ns(paste0("paneltitle", i)))}),#
        # headings = lapply(seq_len(n_panels), function(i) {uiOutput(h4(i))}),#
        collapsible = TRUE, show = TRUE,
        ns = ns
      )
      panel_modules <- lapply(seq_along(content_IDs), function(i) {
        callModule(examplePanel, content_IDs[i], reactive(all_panels$state()[[i]]))
      })
      lapply(seq_along(panel_modules), function(i) {
        output[[paste0("paneltitle", i)]] <- renderflamingoPanelHeading(panel_modules[[i]]())
      })
      observeEvent(input$delete_all, {
        all_panels$remove_all()
      })
    }

    shinyApp(ui = ui, server = server)

  }
}

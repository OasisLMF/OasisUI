# TODOs:
# * Review / enhance the module signature.
# * roxygen tags including examples.
# * Refine UI look (together with flamingoPanel).

flamingoIncrementalPanelUI <- function(id, ..., heading = NULL, footer = NULL, status = "default",
                                       collapsible = FALSE, show = TRUE, removable = TRUE) {
  ns <- NS(id)
  flamingoPanel(
    id = id,
    ...,
    heading = tagList(
      shiny::actionButton(ns("add"), icon("plus"), style = "display: inline-block; float: left; margin-right: 12px"),
      flamingoPanelHeading(heading),
      if(removable) shiny::actionButton(ns("delete"), icon("times"), style = "display: inline-block; float: right")
    ),
    footer = footer,
    status = status,
    collapsible = collapsible,
    show = show
  )
}

flamingoIncrementalPanel <- function(input, output, session, panels_state, new_content_IDs, new_content_fun, ..., collapsible = FALSE, show = TRUE) {
  id <- session$ns(NULL)
  observeEvent(input$add, {
    taken <- panels_state()
    new_i <- head(which(!taken), 1L)
    new_id <- names(taken)[new_i]
    cat("add", new_id, "\n")
    if (length(new_id) > 0) {
      taken[new_id] <- TRUE
      panels_state(taken)
      new_content_id <- new_content_IDs[new_i]
      insertUI(
        sprintf("#%s", id), "beforeBegin",
        flamingoIncrementalPanelUI(new_id, new_content_fun(new_content_id, ...), collapsible = collapsible, show = show)
      )
    } else {
      showNotification("Reached maximum number of panels", type = "warning")
    }
  })
  observeEvent(input$delete, {
    showModal(modalDialog(
      title = "Remove the selected panel?",
      "Do you want to remove the selected panel?",
      footer = tagList(
        modalButton("Cancel"),
        shiny::actionButton(session$ns("del_ok"), "OK")
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

panelsState <- function(IDs) {
  reactiveVal(
    setNames(c(TRUE, rep(FALSE, length(IDs))), IDs)
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
        textOutput(ns("txt_out"))
      )
    }
    examplePanel <- function(input, output, session, active) {
      output$txt_out <- renderText(input$txt_in)
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
      fluidPage(
        flamingoIncrementalPanelUI(
          "extpanel-0", heading = "Add a new panel",
          collapsible = FALSE, show = FALSE, removable = FALSE
        )
      )
    )
    server <- function(input, output, session) {
      # NOTE that, since we are using server logic to create UI elements, the IDs
      # used for the UI components must include session$ns (relevant for module
      # server functions)
      ns <- session$ns
      # track which panels are taken (by name)
      panel_names <- paste0("extpanel-", c(seq_len(n_panels)))
      panels_state <- panelsState(ns(panel_names))
      # content IDs used for the content module server and UI
      content_IDs <- paste0("content-", seq_len(n_panels))
      # panels modules
      lapply(
        c(panel_names, "extpanel-0"),
        callModule, module = flamingoIncrementalPanel,
        panels_state,
        ns(content_IDs), examplePanelUI,
        collapsible = TRUE, show = TRUE
      )
      # content modules
      lapply(content_IDs, callModule, module = examplePanel)
    }

    shinyApp(ui = ui, server = server)

  }
}

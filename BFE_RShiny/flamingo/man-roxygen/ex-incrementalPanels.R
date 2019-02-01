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
    txt <- reactiveVal(NULL)
    observe({
      reset()
      txt(NULL)
    })
    observeEvent(input$upd, txt(input$txt_in))
    output$txt_out <- renderText(txt())
    reactive(txt())
  }
  ui <- fluidPage(
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
      headings = lapply(seq_len(n_panels), function(i) {
        flamingoPanelHeadingOutput(ns(paste0("paneltitle", i)))
      }),
      collapsible = TRUE, show = TRUE,
      ns = ns
    )
    panel_modules <- lapply(seq_along(content_IDs), function(i) {
      callModule(examplePanel, content_IDs[i], reactive(all_panels$state()[[i]]))
    })
    lapply(seq_along(panel_modules), function(i) {
      output[[paste0("paneltitle", i)]] <- renderFlamingoPanelHeading(panel_modules[[i]]())
    })
    observeEvent(input$delete_all, {
      all_panels$remove_all()
    })
  }

  shinyApp(ui = ui, server = server)

}

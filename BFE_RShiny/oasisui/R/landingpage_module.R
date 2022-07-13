# landingPage module -----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' landingPage
#'
#' @rdname landingPage
#'
#' @return List of tags.
#'
#' @importFrom DT DTOutput
#' @importFrom bsplus bs_embed_tooltip
#'
#' @export
landingPageUI <- function(id) {
  ns <- NS(id)

  tagList(
    oasisuiPanel(
      collapsible = FALSE,
      ns("panel_landingpage"),
      heading = tagAppendChildren(
        h4("Analyses table"),
        oasisuiRefreshButton(ns("abuttonrefreshanaInbox"))
      ),
      DTOutput(ns("dt_anaInbox")),
      oasisuiButton(ns("abuttongotoana"), "Dashboard of Analyses Outputs", align = "right") %>%
        bs_embed_tooltip(title = landing_page_tooltips$abuttongotoana, placement = "right"),
      oasisuiTableButton(inputId = ns("abuttondelana"), label = "Delete Analysis") %>%
        bs_embed_tooltip(title = landing_page_tooltips$abuttondelana, placement = "right"),
      downloadButton(ns("downloadexcel_ana"), label = "CSV") %>%
        bs_embed_tooltip(title = landing_page_tooltips$downloadexcel_ana, placement = "right")
    )
  )
}


# Server -----------------------------------------------------------------------

#' Landing Page
#'
#' @rdname landingPage
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-active
#'
#' @return anaID id of selected analysis
#'
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @importFrom data.table fwrite
#'
#' @export
landingPage <- function(input, output, session, active = reactive(TRUE)) {

  # Reactive Values and parameters ---------------------------------------------

  # parameter, number of milliseconds to wait before refreshing tables
  # (300000 == 5 mins)
  reloadMillis <- 300000

  navigation_state <- reactiveNavigation()

  result <- reactiveValues(
    tbl_anaInbox = NULL,
    anaID = NULL
  )

  # navigation -----------------------------------------------------------------
  observeEvent(input$abuttongotoana,{
    updateNavigation(navigation_state, "SBR")
    result$anaID <- result$tbl_anaInbox[input$dt_anaInbox_rows_selected, tbl_analysesDataNames$id]
  })

  # Inbox table ----------------------------------------------------------------

  # Reload Process Runs table
  .reloadAnaData <- function() {
    logMessage(".reloadAnaData landingpage called")
    result$tbl_anaInbox <- session$userData$data_hub$return_tbl_analysesData(Status = Status, tbl_analysesDataNames = tbl_analysesDataNames)
    logMessage("analyses table refreshed")
    invisible()
  }

  observe(if (active()) {
    # Reset Param
    result$anaID <- NULL

    # invalidate if the refresh button updates
    force(input$abuttonrefreshanaInbox)

    #refesh table
    .reloadAnaData()
  })

  output$dt_anaInbox <- renderDT(if (!is.null(result$tbl_anaInbox)) {
    datatable(
      result$tbl_anaInbox %>% session$userData$data_hub$return_tbl_analysesData_nice(admin_mode = getOption("oasisui.settings.admin.mode"), Status = Status, tbl_modelsDataNames = tbl_modelsDataNames, tbl_portfoliosDataNames = tbl_portfoliosDataNames, tbl_analysesDataNames = tbl_analysesDataNames),
      class = "oasisui-table display",
      rownames = FALSE,
      selection = "single",
      filter = 'bottom',
      escape = FALSE,
      plugins = 'natural',
      options = getTableOptions()
    )
  } else {
    nothingToShowTable("No analyses available")
  }
  )

  output$downloadexcel_ana <- downloadHandler(
    filename = "analyses_inbox.csv",
    content = function(file) {
      fwrite(result$tbl_anaInbox, file, row.names = FALSE, quote = TRUE)
    }
  )

  # Delete analysis ------------------------------------------------------------
  observeEvent(input$abuttondelana, {
    showModal(.deleteAna())
  })

  output$deleteAnatitle <- renderUI({
    analysisID <- result$tbl_anaInbox[input$dt_anaInbox_rows_selected, tbl_analysesDataNames$id]
    AnaName <- result$tbl_anaInbox[input$dt_anaInbox_rows_selected, tbl_analysesDataNames$name]
    paste0('Delete analysis ', analysisID, ' ', AnaName)
  })

  .deleteAna <- function(){
    ns <- session$ns
    modalDialog(label = "deleteAnaModal",
                title = uiOutput(ns("deleteAnatitle"), inline = TRUE),
                paste0("Are you sure that you want to delete this analysis?"),
                footer = tagList(
                  oasisuiButton(ns("abuttonConfirmDelAna"),
                                label = "Confirm", align = "center") %>%
                    bs_embed_tooltip(title = landing_page_tooltips$abuttonConfirmDelAna, placement = "right"),
                  actionButton(ns("btnCancelDelAna"),
                               label = "Go back", align = "right")
                ),
                size = "m",
                easyClose = TRUE
    )
  }

  observeEvent(input$abuttonConfirmDelAna, {
    removeModal()
    analysisID <- result$tbl_anaInbox[input$dt_anaInbox_rows_selected, tbl_analysesDataNames$id]
    delete_analyses_id <- session$userData$oasisapi$api_delete_query(query_path = paste("analyses", analysisID, sep = "/"))
    # api_delete_analyses_id(analysisID)
    if (delete_analyses_id$status == "Success") {
      oasisuiNotification(type = "message",
                          paste0("Analysis id ", analysisID, " deleted."))
      .reloadAnaData()
    } else {
      oasisuiNotification(type = "error",
                          paste0("Analysis id ", analysisID, " could not be deleted."))
    }
  })

  observeEvent(input$btnCancelDelAna, {
    removeModal()
  })

  # Refresh button -------------------------------------------------------------
  observeEvent(input$abuttonrefreshanaInbox, {
    withModalSpinner(
      .reloadAnaData(),
      "Refreshing...",
      size = "s", t = 0.5
    )
  })

  # Enable /Disable buttons ----------------------------------------------------
  observeEvent(input$dt_anaInbox_rows_selected, ignoreNULL = FALSE, {
    if (length(input$dt_anaInbox_rows_selected) > 0) {

      enable("abuttondelana")
      if (result$tbl_anaInbox[input$dt_anaInbox_rows_selected, tbl_analysesDataNames$status] == Status$Completed) {
        enable("abuttongotoana")
      } else {
        disable("abuttongotoana")
      }
    } else {
      disable("abuttongotoana")
      disable("abuttondelana")
    }
  })

  # Module Output --------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      anaID = reactive({result$anaID})
    )
  )

  moduleOutput
}

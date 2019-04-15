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
    flamingoPanel(
      collapsible = FALSE,
      ns("panel_landingpage"),
      heading = tagAppendChildren(
        h4("Analyses table"),
        flamingoRefreshButton(ns("abuttonrefreshanaInbox"))
      ),
      DTOutput(ns("dt_anaInbox")),
      flamingoButton(ns("abuttongotoana"), "Dashboard of Analyses Outputs", align = "right") %>%
        bs_embed_tooltip(title = landing_page$abuttongotoana, placement = "right"),
      flamingoTableButton(inputId = ns("abuttondelana"), label = "Delete Analysis") %>%
        bs_embed_tooltip(title = landing_page$abuttondelana, placement = "right"),
      downloadButton(ns("downloadexcel_ana"), label = "Export to csv") %>%
        bs_embed_tooltip(title = landing_page$downloadexcel_ana, placement = "right")
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
#' @template params-logMessage
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
landingPage <- function(input, output, session, logMessage = message, active = reactive(TRUE)) {

  # Reactive Values and parameters ---------------------------------------------

  # parameter, number of milliseconds to wait before refreshing tables
  # (300000 == 5 mins)
  reloadMillis <- 300000

  navigation_state <- reactiveNavigation()

  result <- reactiveValues(
    tbl_anaInbox = NULL,
    anaID = -1
  )

  # navigation -----------------------------------------------------------------
  observeEvent(input$abuttongotoana,{
    updateNavigation(navigation_state, "SBR")
    result$anaID <- result$tbl_anaInbox[input$dt_anaInbox_rows_selected, tbl_analysesDataNames$id]
  })

  # Inbox table ----------------------------------------------------------------

  # Reload Process Runs table
  .reloadAnaData <- function() {
    logMessage(".reloadAnaData called")
    result$tbl_anaInbox <- return_tbl_analysesData()
    logMessage("analyses table refreshed")
    invisible()
  }

  observe(if (active()) {
    # Reset Param
    result$anaID <- -1

    # invalidate if the refresh button updates
    force(input$abuttonrefreshanaInbox)

    # reload automatically every so often
    invalidateLater(reloadMillis)

    #refesh table
    .reloadAnaData()
  })

  output$dt_anaInbox <- renderDT(if (!is.null(result$tbl_anaInbox)) {
    datatable(
      result$tbl_anaInbox %>% return_tbl_analysesData_nice(),
      class = "flamingo-table display",
      rownames = FALSE,
      selection = "single",
      #colnames = c("Row Number" = 1),
      filter = 'bottom',
      escape = FALSE,
      plugins = 'natural',
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0, type = 'natural'))
      )
    )
  } else {
    .nothingToShowTable(contentMessage = "No analyses available")
  }
  )

  output$downloadexcel_ana <- downloadHandler(
    filename = "analyses_inbox.csv",
    content = function(file) {
      fwrite(result$tbl_anaInbox, file, row.names = TRUE, quote = TRUE)
    }
  )

  # Delete analysis ------------------------------------------------------------
  onclick("abuttondelana", {
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
                  flamingoButton(ns("abuttonConfirmDelAna"),
                                 label = "Confirm", align = "center") %>%
                    bs_embed_tooltip(title = landing_page$abuttonConfirmDelAna, placement = "right"),
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
    delete_analyses_id <- api_delete_analyses_id(analysisID)
    if (delete_analyses_id$status == "Success") {
      flamingoNotification(type = "message",
                           paste("Analysis id ", analysisID, " deleted."))
      .reloadAnaData()
    } else {
      flamingoNotification(type = "error",
                           paste("Analysis id ", analysisID, " could not be deleted."))
    }
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

  # Help Functions -------------------------------------------------------------
  #empty table
  .nothingToShowTable <- function(contentMessage){
    datatable(
      data.frame(content = contentMessage),
      class = "flamingo-table display",
      selection = "none",
      rownames = FALSE,
      #filter = 'bottom',
      colnames = c(""),
      escape = FALSE,
      options = list(searchHighlight = TRUE)
    )
  }

  moduleOutput
}

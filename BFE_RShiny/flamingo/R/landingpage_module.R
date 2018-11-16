# landingPage module -----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' landingPage
#'
#' @rdname landingPage
#'
#' @template params-module-ui
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
    wellPanel(
      h4("Analyses Table"),
      DTOutput(ns("dt_anaInbox")),

      flamingoButton(ns("abuttongotoana"), "Dashboard of Analyses Outputs", align = "right") %>%
        bs_embed_tooltip(title = landing_page$abuttongotoana, placement = "right"),

      flamingoButton(inputId = ns("abuttondelana"), label = "Delete Analysis") %>%
        bs_embed_tooltip(title = landing_page$abuttondelana, placement = "right"),

      actionButton(inputId = ns("refreshInbox"), label = "Refresh", align = "right"),

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
#' @template params-flamingo-module
#'
#' @param reloadMillis Amount of time to wait between table updates;
#' see \link{invalidateLater}.
#' @param user reactive expression yielding user
#'
#' @return For \code{landingPage()}, list of reactives:
#' \itemize{
#' 		\item{\code{anaid}: }{id of selected analysis or -1 if nothing is selected}
#' 		\item{\code{modelid}: }{id of selected model or -1 if nothing is selected}
#' }
#'
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#'
#' @export
landingPage <- function(input, output, session, user, dbSettings,
                        reloadMillis = 10000, logMessage = message, active = reactive(TRUE)) {

  # Reactive Values and parameters ---------------------------------------------
  navigation_state <- reactiveNavigation()

  result <- reactiveValues(
    tbl_anaInbox = NULL
  )

  # navigation -----------------------------------------------------------------
  observeEvent(input$abuttongotoana,
               updateNavigation(navigation_state, "SBR"))

  # Inbox table ----------------------------------------------------------------
  
  # Reload Process Runs table
  .reloadAnaData <- function() {
    logMessage(".reloadAnaData called")
    result$tbl_anaInbox <- return_tbl_analysesData()
    logMessage("analyses table refreshed")
    invisible()
  }
  
  observe(if (active()) {
    # invalidate if the refresh button updates
    force(input$refreshInbox)

    # reload automatically every so often
    invalidateLater(reloadMillis)
    
    #refesh table
    .reloadAnaData()
  })

  output$dt_anaInbox <- renderDT(if (!is.null(result$tbl_anaInbox)) {
    datatable(
      result$tbl_anaInbox,
      class = "flamingo-table display",
      rownames = TRUE,
      selection = "single",
      colnames = c("Row Number" = 1),
      filter = 'bottom',
      escape = FALSE,
      plugins = 'natural',
      options = list(
        searchHighlight = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0, type = 'natural'))
      )
    )
  })

  output$downloadexcel_ana <- downloadHandler(
    filename = "analyses_inbox.csv",
    content = function(file) {
      write.csv(result$tbl_anaInbox, file)
    }
  )

  # Delete analysis ------------------------------------------------------------
  observeEvent(input$dt_anaInbox_rows_selected, ignoreNULL = FALSE, {
    if (length(input$dt_anaInbox_rows_selected) > 0) {
      enable("abuttondelana")
    } else {
      disable("abuttondelana")
    }
  })
  
  onclick("abuttondelana", {
    analysisID <- result$tbl_anaInbox[input$dt_anaInbox_rows_selected, tbl_analysesData.AnaID]
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

  ### Module Output ------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      anaid = reactive(if (length(i <- input$dt_anaInbox_rows_selected) == 1) {
        result$tbl_anaInbox[i, 2]} else -1),
      # this is needed in processAna, probably shouldn't
      modelid = reactive(if (length(i <- input$dt_anaInbox_rows_selected) == 1) {
        result$tbl_anaInbox[i, 1]} else -1)
    )
  )

  moduleOutput
}

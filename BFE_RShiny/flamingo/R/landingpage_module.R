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
      h4("Process Runs Inbox"),
      DTOutput(ns("tableInbox")),
      flamingoButton(ns("abuttongotorun"), "Browse Processes Outputs",
                     align = "right") %>%
        bs_embed_tooltip(title = landing_page$abuttongotorun, placement = "right"),
      actionButton(inputId = ns("refreshInbox"), label = "Refresh", align = "right"),
      downloadButton(ns("PRIdownloadexcel"),
                     label = "Export to csv") %>%
        bs_embed_tooltip(title = landing_page$PRIdownloadexcel, placement = "right")
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
#' 		\item{\code{runId}: }{id of selected run or -1 if nothing is selected}
#' 		\item{\code{procId}: }{id of selected process or -1 if nothing is selected}
#' }
#'
#' @importFrom DT renderDT
#' @importFrom DT datatable
#'
#' @export
landingPage <- function(input, output, session, user, dbSettings,
                        reloadMillis = 10000, logMessage = message, active = reactive(TRUE)) {
  
  # Reactive Values and parameters ---------------------------------------------
  navigation_state <- reactiveNavigation()
  
  result <- reactiveValues(
    inbox = NULL,
    runIdList = NULL
  )
  
  # navigation -----------------------------------------------------------------
  observeEvent(input$abuttongotorun,
               updateNavigation(navigation_state, "SBR"))
  
  observe(if (active()) {
    # invalidate if the refresh button updates
    force(input$refreshInbox)
    
    # reload automatically every so often
    invalidateLater(reloadMillis)
    
    landingPageButtonUpdate(session, dbSettings)
    
    data <- getInboxData(dbSettings, user())
    result$inbox <- data %>%
      replaceWithIcons()
    
    logMessage("inbox refreshed")
  })
  
  output$tableInbox <- renderDT(if (!is.null(result$inbox)) {
    datatable(
      result$inbox,
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
  
  output$PRIdownloadexcel <- downloadHandler(
    filename = "processruninbox.csv",
    content = function(file) {
      write.csv(result$inbox, file)
    }
  )
  
  ### Module Output ------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      runId = reactive(if (length(i <- input$tableInbox_rows_selected) == 1) {
        result$inbox[i, 2]} else -1),
      # this is needed in processRun, probably shouldn't
      procId = reactive(if (length(i <- input$tableInbox_rows_selected) == 1) {
        result$inbox[i, 1]} else -1)
    )
  )
  
  moduleOutput
}

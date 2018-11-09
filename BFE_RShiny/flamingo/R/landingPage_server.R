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


#' pageheader
#'
#' @rdname pageheader
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#' 
#' @param reloadMillis Amount of time to wait between table updates;
#' see \link{invalidateLater}.
#' @param user reactive expression yielding user name
#'
#' @return For \code{pageheader()}, list of reactives:
#' \itemize{
#' 		\item{\code{logout}: }{reactive yielding logout button signal}
#' }.
#'
#' @importFrom shinyWidgets toggleDropdownButton
#'
#' @export
pageheader <- function(input, output, session, user, dbSettings,
                       reloadMillis = 10000, logMessage = message, active = reactive(TRUE)) {

  ns <- session$ns

  navigation_state <- reactiveNavigation()

  # Greeter ---------------------------------------------------
  output$textOutputHeaderData2 <- renderText(paste("User Name:", user()))

  observeEvent(input$abuttonuseradmin, {
    updateNavigation(navigation_state, "UA")
    toggleDropdownButton(ns("accountDDmenu"))
  })

  observeEvent(input$abuttondefineaccount, {
    updateNavigation(navigation_state, "DA")
    toggleDropdownButton(ns("accountDDmenu"))
  })

  observeEvent(input$abuttonsysconf, {
    updateNavigation(navigation_state, "SC")
    toggleDropdownButton(ns("accountDDmenu"))
  })

  observeEvent(input$abuttonhome, {
    updateNavigation(navigation_state, "LP")
    toggleDropdownButton(ns("accountDDmenu"))
  })

  .logoutModal <- function() {
    ns <- session$ns
    modalDialog(label = "modaldialoguelogout",
                title = "Important message",
                "Are you sure you want to log out?",
                footer = tagList(
                  flamingoButton(inputId = ns("abuttonlogoutcontinue"), label = "Continue Logout"),
                  modalButton(label = "Dismiss")
                ),
                size = "s",
                easyClose = TRUE)
  }

  observeEvent(input$abuttonlogout,
               showModal(.logoutModal())
  )

  observeEvent(input$abuttonlogoutcontinue,
               removeModal()
  )

  ### Button permissions ---- --------------------------------------------------
  # observe(if (active()) {
  #   landingPageButtonUpdate(session, dbSettings, user())
  # })

  ### Module Output ------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      logout = reactive(input$abuttonlogoutcontinue)
    )
  )

  moduleOutput
}


#' Page Structure
#'
#' @rdname pagestructure
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @param reloadMillis Amount of time to wait between table updates;
#' see \link{invalidateLater}.
#' 
#' @return collapsed status of panel.
#'
#' @importFrom shinyWidgets toggleDropdownButton
#' 
#' @export
pagestructure <- function(input, output, session, dbSettings,
                          reloadMillis = 10000, logMessage = message,
                          active = reactive(TRUE)) {

  ns <- session$ns

  navigation_state <- reactiveNavigation()

  state <- reactiveValues(
    collapsed = FALSE
  )

  observeEvent(input$abuttoncollapsesidebar, {
    state$collapsed <- !state$collapsed
  })

  observe({
    output$sidebar <-
      renderUI(pagestructureSidebar(ns, state$collapsed))
  })


  ### Navigation Menu ----------------------------------------------------------

  observeEvent(input$abuttondefineprogrammesingle, {
    updateNavigation(navigation_state, "PS")
    toggleDropdownButton(ns("abuttonrun"))
  })

  observeEvent(input$abuttondefineprogrammebatch, {
    updateNavigation(navigation_state, "PB")
    toggleDropdownButton(ns("abuttonrun"))
  })

  observeEvent(input$abuttonbrowseSBR, {
    updateNavigation(navigation_state, "SBR")
    toggleDropdownButton(ns("abuttonbrowse"))
  })

  observeEvent(input$abuttonbrowseBBR, {
    updateNavigation(navigation_state, "BBR")
    toggleDropdownButton(ns("abuttonbrowse"))
  })

  observeEvent(input$abuttonbrowseCBR, {
    updateNavigation(navigation_state, "CBR")
    toggleDropdownButton(ns("abuttonbrowse"))
  })


  observeEvent(input$abuttonhome, {
    updateNavigation(navigation_state, "LP")
  })

  observeEvent(input$abuttonfilemngt, {
    updateNavigation(navigation_state, "FM")
  })

  ### Module Output ------------------------------------------------------------
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      collapsed = reactive(state$collapsed)
    ) # placeholder
  )

  moduleOutput

}


#' Landing Page Access Control
#'
#' @rdname landingPageButtonUpdate
#'
#' @description Disable/Enable menu buttons based on permissions in database.
#'
#' @template params-module
#' @template params-flamingo-module
#' 
#' @return NULL
#'
#' @export
landingPageButtonUpdate <- function(session, dbSettings,
                                    logMessage = message) {

  logMessage("Checking Permissions")

  # TODO: use shinyjs enable / disable on actionButtons according to permissions

  # .updateButton <- function(db_resourceId, btn_inputId) {
  #   permission <- flamingoDBCheckPermissions(dbSettings, user, db_resourceId)
  #   if (identical(permission, character(0))) {
  #     updateButton(session, session$ns(btn_inputId), disabled = TRUE)
  #   } else {
  #     updateButton(session, session$ns(btn_inputId), disabled = TRUE)
  #   }
  # }

  # Not used anywhere else, probably just forgotten
  # ("600", "abuttonenquiry")

  #.updateButton("700", "abuttonrun")
  #.updateButton("700", "abuttonbrowse")

  # .updateButton("904", "abuttonuseradmin")

  # Not used anywhere else, probably just forgotten
  # ("950", "abuttonworkflowadmin")

  # .updateButton("200", "abuttonsysconf")

  #.updateButton("300", "abuttonfilemngt")

  invisible()
}

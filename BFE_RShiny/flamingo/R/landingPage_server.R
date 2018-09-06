#' Landing Page
#' @rdname landingPage
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @param userId reactive expression yielding user id
#' @param userName reactive expression yielding user name
#' @return For \code{landingPage()}, list of reactives:
#' \itemize{
#' 		\item{\code{runId}: }{id of selected run or -1 if nothing is selected}
#' 		\item{\code{procId}: }{id of selected process or -1 if nothing is selected}
#' }
#' @template return-outputNavigation
#' @importFrom DT renderDataTable datatable
#' @importFrom dplyr mutate '%>%'
#' @importFrom utils write.csv
#' @importFrom shinyWidgets dropdown toggleDropdownButton
#' @export
landingPage <- function(input, output, session, userId, userName, dbSettings,
                        reloadMillis = 10000, logMessage = message, active = reactive(TRUE)) {

  navigation_state <- reactiveNavigation()

  result <- reactiveValues(
    inbox = NULL,
    runIdList = NULL
  )

  observeEvent(input$abuttongotorun,
               updateNavigation(navigation_state, "WF"))

  observe(if (active()) {

    # invalidate if the refresh button updates
    force(input$refreshInbox)

    # reload automatically every so often
    invalidateLater(reloadMillis)

    landingPageButtonUpdate(session, dbSettings, userId())

    data <- getInboxData(dbSettings, userId())
    result$inbox <-  data %>%
      mutate(Status = replace(Status, Status == "Failed" | Status == "Cancelled", StatusFailed)) %>%
      mutate(Status = replace(Status, Status == "Completed", StatusCompleted)) %>%
      mutate(Status = replace(Status, Status != "Completed" & Status != "Failed" & Status != "Cancelled" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
      as.data.frame()

    logMessage("inbox refreshed")

    result$runIdList <- result$inbox[, c("RunID", "Status")]
    #logMessage(paste0("result$runIdList ", names(result$runIdList)))
  })

  output$tableInbox <- renderDataTable(if (userId() != FLAMINGO_GUEST_ID) {

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

  ### Module Output ----
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      runId = reactive(if (length(i <- input$tableInbox_rows_selected) == 1) {
        result$inbox[i, 2]} else -1),
      # this is needed in processRun, probably shouldn't
      procId = reactive(if (length(i <- input$tableInbox_rows_selected) == 1) {
        result$inbox[i, 1]} else -1),
      runIdList = reactive(result$runIdList)
    )
    
  )
  
  moduleOutput

}


#' pageheader
#' @rdname pageheader
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @param userId reactive expression yielding user id
#' @param userName reactive expression yielding user name
#' @return For \code{pageheader()}, list of reactives:
#' \itemize{
#' 		\item{\code{logout}: }{reactive yielding logout button signal}
#' }
#' @template return-outputNavigation
#' @importFrom shinyWidgets dropdown toggleDropdownButton
#' @export
pageheader <- function(input, output, session, userId, userName, dbSettings,
                       reloadMillis = 10000, logMessage = message, active = reactive(TRUE)) {

  ns <- session$ns

  navigation_state <- reactiveNavigation()

  ### Greeter ----
  output$textOutputHeaderData2 <- renderText(paste("User Name:", userName()))

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

  LogoutModal <- function(){
    ns <- session$ns
    modalDialog(label = "modaldialoguelogout",
                title = "Important message",
                "Are you sure you want to log out?",
                footer = tagList(
                  actionButton(inputId = ns("abuttonlogoutcontinue"), label = "Continue Logout", class = "btn btn-primary"),
                  modalButton(label = "Dismiss")
                ),
                size = "s",
                easyClose = TRUE)
  }

  observeEvent(input$abuttonlogout,
               showModal(LogoutModal())
  )

  observeEvent(input$abuttonlogoutcontinue,
               removeModal()
  )

  ### Button permissions ----
  observe(if (active()) {
    landingPageButtonUpdate(session, dbSettings, userId())
  })

  ### Module Output ----
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      # logout = reactive(input$abuttonlogout),
      logout = reactive(input$abuttonlogoutcontinue)
    )
  )

  moduleOutput

}


#' Page Structure
#' @rdname pagestructure
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @param userId reactive expression yielding user id
#' @param userName reactive expression yielding user name
#' @return For \code{pagestructure()}, list of reactives.
#' @template return-outputNavigation
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyWidgets panel tooltipOptions toggleDropdownButton
#' @export
pagestructure <- function(input, output, session, userId, userName, dbSettings,
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


  ### Navigation Menu ----

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
  
  observeEvent(input$abuttonhome, {
    updateNavigation(navigation_state, "LP")
  })

  observeEvent(input$abuttonfilemngt, {
    updateNavigation(navigation_state, "FM")
  })


  ### Button permissions ----

  observe(if (active()) {
    invalidateLater(reloadMillis)
    landingPageButtonUpdate(session, dbSettings, userId())
  })

  ### Module Output ----
  moduleOutput <- c(
    outputNavigation(navigation_state),
    list(
      collapsed = reactive(state$collapsed)
    ) # placeholder
  )

  moduleOutput

}


#' sidebar_button
#' @description function containing the sidebar UI
#' @inheritParams pagestructure
#' @importFrom shinyBS bsButton
#' @export
sidebar_button <- function(ID, Label = NULL, Icon = NULL, Block = TRUE, Style = "btn btn-primary"){
  fluidRow(
    bsButton(inputId = ID, label = Label, icon = Icon, style = Style,
             color = "primary", size = "default", type = "action", block = Block),
    style = "margin:2%;"
  )
}


#' Landing Page Access Control
#' @description Disable/Enable menu buttons based on permissions in database
#' @inheritParams pagestructure
#' @importFrom shinyBS updateButton
#' @export
landingPageButtonUpdate <- function(session, dbSettings, userId,
                                    logMessage = message) {

  logMessage("Checking Permissions")

  if (userId == FLAMINGO_GUEST_ID) return(NULL)

  .updateButton <- function(db_resourceId, btn_inputId) {
    permission <- flamingoDBCheckPermissions(dbSettings, userId, db_resourceId)
    if (identical(permission, character(0))) {
      updateButton(session, btn_inputId, disabled = TRUE)
    } else {
      updateButton(session, btn_inputId, disabled = FALSE)
    }
  }

  # Not used anywhere else, probably just forgotten
  # ("600", "abuttonenquiry")

  .updateButton("700", "abuttonrun")
  .updateButton("700", "abuttonbrowse")

  .updateButton("904", "abuttonuseradmin")

  # Not used anywhere else, probably just forgotten
  # ("950", "abuttonworkflowadmin")

  .updateButton("200", "abuttonsysconf")

  .updateButton("300", "abuttonfilemngt")

  invisible()
}

#' Landing Page
#' @rdname landingPage
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @param userId reactive expression yielding user id
#' @param userName reactive expression yielding user name
#' @return list of reactive expressions
#' \itemize{
#' 		\item{\code{navigate}: }{reactive yielding navigation}
#' 		\item{\code{runId}: }{id of selected run or -1 if nothing is selected}
#' 		\item{\code{procId}: }{id of selected process or -1 if nothing is selected}
#' }
#' @importFrom DT renderDataTable datatable
#' @importFrom dplyr mutate '%>%'
#' @importFrom utils write.csv
#' @importFrom shinyWidgets dropdown
#' @export
landingPage <- function(input, output, session, userId, userName, dbSettings,
                        reloadMillis = 10000, logMessage = message, active = reactive(TRUE)) {

  result <- reactiveValues(
    navigate = NULL,
    inbox = NULL
  )

  observeEvent(input$abuttongotorun,
               result$navigate <- structure("WF", count = input$abuttongotorun))

  observe(if (active()) {

    # invalidate if the refresh button updates
    force(input$refreshInbox)

    # reload automatically every so often
    invalidateLater(reloadMillis)

    landingPageButtonUpdate(session, dbSettings, userId())

    result$inbox <- getInboxData(dbSettings, userId()) %>%
      mutate(Status = replace(Status, Status == "Failed" | Status == "Cancelled", StatusFailed)) %>%
      mutate(Status = replace(Status, Status == "Completed", StatusCompleted)) %>%
      mutate(Status = replace(Status, Status != "Completed" & Status != "Failed" & Status != "Cancelled" & Status != StatusFailed & Status != StatusCompleted, StatusProcessing)) %>%
      as.data.frame()

    logMessage("inbox refreshed")

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
    filename ="processruninbox.csv",
    content = function(file) {
      write.csv(result$inbox, file)
    }
  )

  ### Module Output ----
  moduleOutput <- list(
    navigate = reactive(result$navigate),
    runId = reactive(if (length(i <- input$tableInbox_rows_selected) == 1) {
      result$inbox[i, 2]} else -1),
    # this is needed in processRun, probably shouldn't
    procId = reactive(if (length(i <- input$tableInbox_rows_selected) == 1) {
      result$inbox[i, 1]} else -1)
  )

  return(moduleOutput)

}


#' pageheader
#' @rdname pageheader
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @param userId reactive expression yielding user id
#' @param userName reactive expression yielding user name
#' @return list of reactive expressions
#' \itemize{
#' 		\item{\code{navigate}: }{reactive yielding navigation}
#' 		\item{\code{logout}: }{reactive yielding logout button signal}
#' }
#' @importFrom shinyWidgets dropdown toggleDropdownButton
#' @export
pageheader <- function(input, output, session, userId, userName, dbSettings,
                       reloadMillis = 10000, logMessage = message, active = reactive(TRUE)) {

  ns <- session$ns

  result <- reactiveValues(
    navigate = NULL
  )

  ### Greeter ----
  output$textOutputHeaderData2 <- renderText(paste("User Name:", userName()))

  observeEvent(input$abuttonuseradmin,{
               result$navigate <- structure("UA", count = input$abuttonuseradmin)
               toggleDropdownButton(ns("accountDDmenu"))})

  observeEvent(input$abuttondefineaccount,{
               result$navigate <- structure("DA", count = input$abuttondefineaccount)
               toggleDropdownButton(ns("accountDDmenu"))})

  observeEvent(input$abuttonsysconf,{
               result$navigate <- structure("SC", count = input$abuttonsysconf)
               toggleDropdownButton(ns("accountDDmenu"))})

  observeEvent(input$abuttonhome,{
               result$navigate <- structure("LP", count = input$abuttonhome)
               toggleDropdownButton(ns("accountDDmenu"))})


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
  moduleOutput <- list(
    navigate = reactive(result$navigate),
    # logout = reactive(input$abuttonlogout),
    logout = reactive(input$abuttonlogoutcontinue)
  )

  return(moduleOutput)

}


#' Page Structure
#' @rdname pagestructure
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @param userId reactive expression yielding user id
#' @param userName reactive expression yielding user name
#' @param collapsed reactive expression determining how the UI should be rendered
#' @return list of reactive expressions
#' \itemize{
#' 		\item{\code{navigate}: }{reactive yielding navigation}
#' }
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyWidgets panel tooltipOptions toggleDropdownButton
#' @export
pagestructure <- function(input, output, session, userId, userName, dbSettings,
                          reloadMillis = 10000, logMessage = message,
                          active = reactive(TRUE), collapsed = reactive(FALSE)) {

  ns <- session$ns

  result <- reactiveValues(
    navigate = NULL
  )

  observe({
    output$sidebar <-
      renderUI(pagestructureSidebar(ns, collapsed = collapsed()))
  })


  ### Navigation Menu ----

  observeEvent(input$abuttondefineprogrammesingle,{
               result$navigate <- structure("PS", count = input$abuttondefineprogrammesingle)
               toggleDropdownButton(ns("abuttonrun"))})

  observeEvent(input$abuttondefineprogrammebatch,{
               result$navigate <- structure("PB", count = input$abuttondefineprogrammebatch)
               toggleDropdownButton(ns("abuttonrun"))})

  observeEvent(input$abuttonbrowse,
               result$navigate <- structure("BR", count = input$abuttonbrowse))

  observeEvent(input$abuttonhome,
               result$navigate <- structure("LP", count = input$abuttonhome))

  observeEvent(input$abuttonexpmngt,
               result$navigate <- structure("EM", count = input$abuttonexpmngt))

  observeEvent(input$abuttonprmngt,
               result$navigate <- structure("WF", count = input$abuttonprmngt))

  observeEvent(input$abuttonfilemngt,
               result$navigate <- structure("FM", count = input$abuttonfilemngt))


  ### Button permissions ----

  observe(if (active()) {
    invalidateLater(reloadMillis)
    landingPageButtonUpdate(session, dbSettings, userId())
  })

  ### Module Output ----
  moduleOutput <- list(
    navigate = reactive(result$navigate)
  )

  return(moduleOutput)

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

  .updateButton("1000", "abuttonprmngt")

  .updateButton("700", "abuttonexpmngt")

  .updateButton("700", "abuttonrun")

  .updateButton("904", "abuttonuseradmin")

  # Not used anywhere else, probably just forgotten
  # ("950", "abuttonworkflowadmin")

  .updateButton("200", "abuttonsysconf")

  .updateButton("300", "abuttonfilemngt")

  invisible()
}

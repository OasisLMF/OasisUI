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
#' @importFrom DT renderDataTable
#' @importFrom  shinyBS bsTooltip
#' @importFrom dplyr mutate
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

      filename = "processruninbox.csv",
      content = function(file) {
        write.csv(result$inbox, file)
      }

  )
  
  ### Module Output
  
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
#' @export

pageheader <- function(input, output, session, userId, userName, dbSettings,
                          reloadMillis = 10000, logMessage = message, active = reactive(TRUE)) {
  
  ns <- session$ns
  
  result <- reactiveValues(
    navigate = NULL
  )
  
  ### Greeter
  
  output$textOutputHeaderData2 <- renderText(paste("User Name:", userName()))
  
  observeEvent(input$abuttonuseradmin,
               result$navigate <- structure("UA", count = input$abuttonuseradmin))#,
               #shinyjs::hide(id = "accountDDmenu"))
  
  observeEvent(input$abuttonsysconf,
               result$navigate <- structure("SC", count = input$abuttonsysconf))
  
  observeEvent(input$abuttonhome,
               result$navigate <- structure("LP", count = input$abuttonhome))#,
              #shinyjs::hide(id = "accountDDmenu"))
  
  
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
  
  observeEvent(input$abuttonlogoutcontinue, {
    removeModal()
  })
  
  ### Button permissions
  
  observe(if (active()) {
    landingPageButtonUpdate(session, dbSettings, userId())
  })
  
  
  
  ### Module Output
  
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
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyWidgets dropdownButton
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @param userId reactive expression yielding user id
#' @param userName reactive expression yielding user name
#' @param W reactive expressione yelding the Width of the sidebar
#' @return list of reactive expressions
#' \itemize{
#' 		\item{\code{navigate}: }{reactive yielding navigation}
#' }
#' @export
pagestructure <- function(input, output, session, userId, userName, dbSettings,
    reloadMillis = 10000, logMessage = message, active = reactive(TRUE), W) {
 
   ns <- session$ns
  
  result <- reactiveValues(
      navigate = NULL,
      Width = 9
  )
  
  observe(if (!is.null(W())) {
    result$Width <- W()
  })

  
  ### Sidebar

  
  sidebarExtended <- panel(
    heading = sidebar_button(ID = ns("abuttonhome"),  Icon = icon("home"), Block = FALSE),
    # sidebar_button(ID = ns("abuttonrun"),  Label = "Run"),
    # bsTooltip(ns("abuttonrun"), 
    #           landing_page$abuttonrun, 
    #           placement = "right", 
    #           options   = list(container = "body")),
    dropdown(
      inputId = ns("abuttonrun"),
      status = "primary",
      label = "Run",
      circle = FALSE,
      tooltip = tooltipOptions(title = landing_page$abuttonrun, placement = "right"),
      actionButton(ns("abuttondefineaccount"), "Define Account",
                   class = "btn btn-primary", align = "right",  width = "100%"),
      bsTooltip(ns("abuttondefineaccount"),
                landing_page$abuttondefineaccount,
                placement = "right",
                options   = list(container = "body")),
      dropdown(
        inputId = ns("abuttondefineprogramme"),
        label = "Define Programme",
        status = "primary",
        circle = FALSE,
        tooltip = tooltipOptions(title = landing_page$abuttondefineprogramme, placement = "right"),
        actionButton(ns("abuttondefineprogrammesingle"), "Define Single Programme",
                     class = "btn btn-primary", align = "right",  width = "100%"),
        bsTooltip(ns("abuttondefineprogrammesingle"),
                  landing_page$abuttondefineprogrammesingle,
                  placement = "right",
                  options   = list(container = "body")),
        actionButton(ns("abuttondefineprogrammebatch"), "Define Batch Programme",
                     class = "btn btn-primary", align = "right",  width = "100%"),
        bsTooltip(ns("abuttondefineprogrammebatch"),
                  landing_page$abuttondefineprogrammebatch,
                  placement = "right",
                  options   = list(container = "body"))
      )
    ),
    sidebar_button(ID = ns("abuttonbrowse"),  Label = "Browse"),
    bsTooltip(ns("abuttonbrowse"), 
              landing_page$abuttonbrowse, 
              placement = "right", 
              options   = list(container = "body")),
    sidebar_button(ID = ns("abuttonexpmngt"),  Label = "Exposure Management"),
    bsTooltip(ns("abuttonexpmngt"), 
              landing_page$abuttonexpmngt, 
              placement = "right", 
              options   = list(container = "body")),
    sidebar_button(ID = ns("abuttonprmngt"),   Label = "Process Management"),
    bsTooltip(ns("abuttonprmngt"), 
              landing_page$abuttonprmngt, 
              placement = "right", 
              options   = list(container = "body")),
    sidebar_button(ID = ns("abuttonfilemngt"), Label = "File Management"),
    bsTooltip(ns("abuttonfilemngt"), 
              landing_page$abuttonfilemngt, 
              placement = "right", 
              options   = list(container = "body"))#,
    # sidebar_button(ID = ns("abuttonsysconf"),  Label = "System Configuration"),
    # bsTooltip(ns("abuttonsysconf"), 
    #           landing_page$abuttonsysconf, 
    #           placement = "right", 
    #           options   = list(container = "body"))
  )
  
  sidebarCollapsed <- panel(
    heading = sidebar_button(ID = ns("abuttonhome"),  Icon = icon("home"), Block = TRUE), 
    dropdown(
      inputId = ns("abuttonrun"),
      status = "primary",
      circle = FALSE,
      tooltip = tooltipOptions(title = landing_page$abuttonrun, placement = "right"),
      actionButton(ns("abuttondefineaccount"), "Define Account",
                   class = "btn btn-primary", align = "right", width = "100%"),
      bsTooltip(ns("abuttondefineaccount"),
                landing_page$abuttondefineaccount,
                placement = "right",
                options   = list(container = "body")),
      dropdown(
        inputId = ns("abuttondefineprogramme"),
        label = "Define Process",
        status = "primary",
        circle = FALSE,
        tooltip = tooltipOptions(title = landing_page$abuttondefineprogramme, placement = "right"),
        actionButton(ns("abuttondefineprogrammesingle"), "Define Single Process",
                     class = "btn btn-primary", align = "right",  width = "100%"),
        bsTooltip(ns("abuttondefineprogrammesingle"),
                  landing_page$abuttondefineprogrammesingle,
                  placement = "right",
                  options   = list(container = "body")),
        actionButton(ns("abuttondefineprogrammebatch"), "Define Batch Process",
                     class = "btn btn-primary", align = "right",  width = "100%"),
        bsTooltip(ns("abuttondefineprogrammebatch"),
                  landing_page$abuttondefineprogrammebatch,
                  placement = "right",
                  options   = list(container = "body"))
      )
    ),
    sidebar_button(ID = ns("abuttonbrowse"),   Icon = icon("eye")),
    bsTooltip(ns("abuttonbrowse"), 
              landing_page$abuttonbrowse, 
              placement = "right", 
              options   = list(container = "body")),
    sidebar_button(ID = ns("abuttonexpmngt"),  Label = "EM"),
    bsTooltip(ns("abuttonexpmngt"), 
              landing_page$abuttonexpmngt, 
              placement = "right", 
              options   = list(container = "body")),
    sidebar_button(ID = ns("abuttonprmngt"),   Label = "PM"),
    bsTooltip(ns("abuttonprmngt"), 
              landing_page$abuttonprmngt, 
              placement = "right", 
              options   = list(container = "body")),
    sidebar_button(ID = ns("abuttonfilemngt"), Label = "FM"),
    bsTooltip(ns("abuttonfilemngt"), 
              landing_page$abuttonfilemngt, 
              placement = "right", 
              options   = list(container = "body"))#,
    # sidebar_button(ID = ns("abuttonsysconf"),  Label = "SC"),
    # bsTooltip(ns("abuttonsysconf"), 
    #           landing_page$abuttonsysconf, 
    #           placement = "right", 
    #           options   = list(container = "body"))
    )

  output$sidebar <- renderUI({sidebarExtended })

  
  observe(if ( result$Width == 9 ) {
    output$sidebar <- renderUI({sidebarExtended})
  } else {
    output$sidebar <- renderUI({sidebarCollapsed})
  })
  
  
  
  ### Navigation Menu
  
  observeEvent(input$abuttondefineaccount,
               result$navigate <- structure("DA", count = input$abuttondefineaccount))#,
               #shinyjs::hide(id = "abuttonrun"))
  
  observeEvent(input$abuttondefineprogrammesingle,
               result$navigate <- structure("PS", count = input$abuttondefineprogrammesingle))#,
               #shinyjs::hide(id = "abuttonrun"))
  
  observeEvent(input$abuttondefineprogrammebatch,
               result$navigate <- structure("PB", count = input$abuttondefineprogrammebatch))#,
  #shinyjs::hide(id = "abuttonrun"))
  
  observeEvent(input$abuttonbrowse,
               result$navigate <- structure("BR", count = input$abuttonbrowse))#,
  #shinyjs::hide(id = "abuttonrun"))
  
  observeEvent(input$abuttonhome,
               result$navigate <- structure("LP", count = input$abuttonhome))
  
  observeEvent(input$abuttonexpmngt,
      result$navigate <- structure("EM", count = input$abuttonexpmngt))
  
  observeEvent(input$abuttonprmngt,
      result$navigate <- structure("WF", count = input$abuttonprmngt))
  
  observeEvent(input$abuttonfilemngt,
      result$navigate <- structure("FM", count = input$abuttonfilemngt))

   
  ### Button permissions
  
  observe(if (active()) {
    invalidateLater(reloadMillis)
    landingPageButtonUpdate(session, dbSettings, userId())
   })
  
  
  
  ### Module Output
  
  moduleOutput <- list(
      navigate = reactive(result$navigate)
   )
  
  return(moduleOutput)
  
}

#@inheritParams pagestructure
#' sidebar_button
#' @description function containing the sidebar UI
#' @inheritParams pagestructure
#' @export

sidebar_button <- function(ID, Label = NULL, Icon = NULL, Block = TRUE, Style = "btn btn-primary"){
  fluidRow(
    bsButton(inputId = ID, label = Label, icon = Icon,
              style = Style, color = "primary", size = "default", type = "action",
              block = Block),
  style = "margin:2%;"
  )
}


#' Landing Page Access Control
#' @description Disable/Enable menu buttons based on permissions in database
#' @inheritParams pagestructure
#' @export
landingPageButtonUpdate <- function(session, dbSettings, userId,
    logMessage = message) {
  
  logMessage("Checking Permissions")
  
  if (userId == FLAMINGO_GUEST_ID) return(NULL)
  
  # Not used anywhere else, probably just forgotten
  # permission <- flamingoDBCheckPermissions(dbSettings, userId, "600")
  # if (identical(permission, character(0))){
  #   updateButton(session, "abuttonenquiry", disabled = TRUE)}
  # else{updateButton(session, "abuttonenquiry", disabled = FALSE)}
  # 
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "1000")
  if (identical(permission, character(0))) {
    updateButton(session, "abuttonprmngt", disabled = TRUE)}
  else {updateButton(session, "abuttonprmngt", disabled = FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "700")
  if (identical(permission, character(0))) {
    updateButton(session, "abuttonexpmngt", disabled = TRUE)}
  else {updateButton(session, "abuttonexpmngt", disabled = FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "700")
  if (identical(permission, character(0))) {
    updateButton(session, "abuttonrun", disabled = TRUE)}
  else {updateButton(session, "abuttonrun", disabled = FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "904")
  if (identical(permission, character(0))) {
    updateButton(session, "abuttonuseradmin", disabled = TRUE)}
  else {updateButton(session, "abuttonuseradmin", disabled = FALSE)}
  
  # Not used anywhere else, probably just forgotten
  # permission <- flamingoDBCheckPermissions(dbSettings, userId, "950")
  # if (identical(permission, character(0))) {
  #   updateButton(session, "abuttonworkflowadmin", disabled = TRUE)}
  # else {updateButton(session, "abuttonworkflowadmin", disabled = FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "200") 
  if (identical(permission, character(0))) {
    updateButton(session, "abuttonsysconf", disabled = TRUE)}
  else {updateButton(session, "abuttonsysconf", disabled = FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "300")
  if (identical(permission, character(0))) {
    updateButton(session, "abuttonfilemngt", disabled = TRUE)}
  else {updateButton(session, "abuttonfilemngt", disabled = FALSE)}
  
}

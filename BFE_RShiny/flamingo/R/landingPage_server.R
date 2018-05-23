
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
#' 		\item{\code{logout}: }{reactive yielding logout button signal}
#' 		\item{\code{runId}: }{id of selected run or -1 if nothing is selected}
#' 		\item{\code{procId}: }{id of selected process or -1 if nothing is selected}
#' }
#' @importFrom DT renderDataTable
#' @export
landingPage <- function(input, output, session, userId, userName, dbSettings,
    reloadMillis = 10000, logMessage = message, active = reactive(TRUE)) {
  
  result <- reactiveValues(
      navigate = NULL,
      inbox = NULL
  )
  
  ### Greeter
  
  output$textOutputHeaderData2 <- renderText(paste("User Name:", userName()))
  
  
  ### Navigation Menu
  
  observeEvent(input$abuttonexpmngt,
      result$navigate <- structure("EM", count = input$abuttonexpmngt))
  
  observeEvent(input$abuttonprmngt,
      result$navigate <- structure("WF", count = input$abuttonprmngt))
  
  observeEvent(input$abuttonfilemngt,
      result$navigate <- structure("FM", count = input$abuttonfilemngt))
  
  observeEvent(input$abuttonsysconf,
      result$navigate <- structure("SC", count = input$abuttonsysconf))
  
  observeEvent(input$abuttonuseradmin,
      result$navigate <- structure("UA", count = input$abuttonuseradmin))
  
  observeEvent(input$abuttongotorun,
      result$navigate <- structure("WF", count = input$abuttongotorun))
  
  
  ### Inbox
  
  observe(if (active()) {
          
        # invalidate if the refresh button updates
        force(input$refreshInbox)
        
        # reload automatically every so often
        invalidateLater(reloadMillis)
        
        result$inbox <- getInboxData(dbSettings, userId())
        
        logMessage("inbox refreshed")
        
      })
  
  output$tableInbox <- renderDataTable(if (userId() != FLAMINGO_GUEST_ID) {
        
        datatable(
            result$inbox,
            class = "flamingo-table display",
            rownames = TRUE,
            selection = "single",
            colnames = c("Row Number" = 1),
            options = list(
                columnDefs = list(list(visible = FALSE, targets = 0))
            )
        )
        
      })
  
  output$PRIdownloadexcel <- downloadHandler(
      
      filename ="processruninbox.csv",
      content = function(file) {
        write.csv(result$inbox, file)
      }
  
  )
  
  
  
  ### Button permissions
  
  observe(if (active()) {
        landingPageButtonUpdate(session, dbSettings, userId())
      })
  
  
  
  ### Module Output
  
  moduleOutput <- list(
      navigate = reactive(result$navigate),
      
      logout = reactive(input$abuttonlogout),
      
      runId = reactive(if(length(i <- input$tableInbox_rows_selected) == 1) {
            result$inbox[i, 2]} else -1),
  
      procId = reactive(if(length(i <- input$tableInbox_rows_selected) == 1) {
            result$inbox[i, 1]} else -1)
  )
  
  return(moduleOutput)
  
}

#' Landing Page Access Control
#' @description Disable/Enable menu buttons based on permissions in database
#' @inheritParams landingPage
#' @export
landingPageButtonUpdate <- function(session, dbSettings, userId,
    logMessage = message) {
  
  logMessage("Checking Permissions")
  
  if (userId == FLAMINGO_GUEST_ID) return(NULL)
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "600")
  if(identical(permission, character(0))){
    updateButton(session, "abuttonenquiry", disabled=TRUE)}
  else{updateButton(session, "abuttonenquiry", disabled=FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "1000")
  if(identical(permission, character(0))){
    updateButton(session, "abuttonprmngt", disabled=TRUE)}
  else{updateButton(session, "abuttonprmngt", disabled=FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "700")
  if(identical(permission, character(0))){
    updateButton(session, "abuttonexpmngt", disabled=TRUE)}
  else{updateButton(session, "abuttonexpmngt", disabled=FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "904")
  if(identical(permission, character(0))){
    updateButton(session, "abuttonuseradmin", disabled=TRUE)}
  else{updateButton(session, "abuttonuseradmin", disabled=FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "950")
  if(identical(permission, character(0))){
    updateButton(session, "abuttonworkflowadmin", disabled=TRUE)}
  else{updateButton(session, "abuttonworkflowadmin", disabled=FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "200") 
  if(identical(permission, character(0))){
    updateButton(session, "abuttonsysconf", disabled=TRUE)}
  else{updateButton(session, "abuttonsysconf", disabled=FALSE)}
  
  permission <- flamingoDBCheckPermissions(dbSettings, userId, "300")
  if(identical(permission, character(0))){
    updateButton(session, "abuttonfilemngt", disabled=TRUE)}
  else{updateButton(session, "abuttonfilemngt", disabled=FALSE)}
  
}

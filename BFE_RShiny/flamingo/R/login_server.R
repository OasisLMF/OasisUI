
#' Id to use for an unidentified user
#' @name FLAMINGO_GUEST_ID
#' @export
FLAMINGO_GUEST_ID <- -1

#' Login Dialog Module
#' @description Server logic to login an user 
#' @inheritParams accountDefinition
#' @param logout reactive yielding logout signal
#' @return list of reactive expressions
#' \itemize{
#' 		\item{\code{userId}: }{yielding an user id if login has been completed
#' 					successfully and \link{FLAMINGO_GUEST_ID} otherwise}
#' 		\item{\code{logout}: }{reactive yielding logout button signal}
#' }
#' @rdname loginDialog 
#' @export
loginDialog <- function(input, output, session, dbSettings, logout,
    logMessage = message, logError = logMessage) {
  
  result <- reactiveValues(
      userId = FLAMINGO_GUEST_ID,
      userName = ""
  )
  
  observeEvent(logout(), {
        result$userId <- FLAMINGO_GUEST_ID
        updateTextInput(session, "userid", label = "", value = "")
        updateTextInput(session, "password", label = "", value = "")
      })
  
  observeEvent(input$loginbutton, {

        if (input$loginbutton > 0) {
          
          userId <- FLAMINGO_GUEST_ID
          
          tryCatch({
                userId <- flamingoDBLogin(
                    dbSettings,
                    pwd = isolate(input$password),
                    uid = isolate(input$userid))
              }, error = function(e) {
                logError(e$message)
              })
          
          if ( userId == FLAMINGO_GUEST_ID ) {
            
            showNotification("Login Failed, please check your credentials.",
                type = "error")
            
          } else {
            
            stmt <- paste("SELECT BFEUserName FROM [dbo].[BFEUser] WHERE BFEUserID =", userId)
            result$userName <- executeDbQuery(dbSettings, stmt)[1,1]
            
          }
          
          result$userId <- userId
          
        }

        logMessage(paste("In Login Userid: ", result$userId))  
        
      })
  
  
  ### Module Output
  
  moduleOutput <- list(
      userId = reactive(result$userId),
      userName = reactive(result$userName)
  )
  
  return(moduleOutput)
}


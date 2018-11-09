
#' FLAMINGO_GUEST_ID
#'
#' @rdname FLAMINGO_GUEST_ID
#'
#' @description Id to use for an unidentified user.
#'
#' @return Set value.
#'
#' @export
FLAMINGO_GUEST_ID <- "unauthorized"

#' loginDialog
#'
#' @rdname loginDialog
#'
#' @description Server logic to login an user.
#' 
#'@template params-module
#'
#' @param dbSettings as returned from \link{flamingoDB}
#' @param logMessage function that will be passed info messages.
#' @param logError function that will be passed error messages.
#' @param logout Reactive yielding logout signal.
#'
#' @return List of reactive expressions:
#' \itemize{
#' 		\item{\code{user}: }{yielding an user id if login has been completed
#' 					successfully and \link{FLAMINGO_GUEST_ID} otherwise}
#' 		\item{\code{logout}: }{reactive yielding logout button signal}
#' }.
#'
#' @importFrom httr content
#' @importFrom shinyjs js
#'
#' @export
loginDialog <- function(input, output, session, dbSettings, logout,
    logMessage = message, logError = logMessage) {

  result <- reactiveValues(
      user = FLAMINGO_GUEST_ID
  )

  observeEvent(logout(), {
    js$reset()
    result$user <- FLAMINGO_GUEST_ID
    updateTextInput(session, "user", label = "", value = "")
    updateTextInput(session, "password", label = "", value = "")
  })

  observeEvent(input$abuttonloginbutton, {
    if (input$abuttonloginbutton > 0) {
      user <- isolate(input$user)
      pwd <- isolate(input$password)
      res <- api_refresh_token(user, pwd)
      if (res$status == "Success") {
        result$user <- 1 #TODO for now, this is a workaround
        #result$user <- user # for later
        options(flamingo.settings.api.token = content(res$result)$access_token)
      } else {
        options(flamingo.settings.api.token = NULL)
        flamingoNotification("Login Failed, please check your credentials.",
                              type = "error")
      }
    }
    logMessage(paste("In Login User: ", result$user))
  })

  # Module Output
  moduleOutput <- list(
    user = reactive(result$user)
  )

  return(moduleOutput)
}

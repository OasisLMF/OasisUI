
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
loginDialog <- function(input, output, session, logout) {

  result <- reactiveValues(
    user = FLAMINGO_GUEST_ID
  )

  observeEvent(logout(), {
    js$reset()
    result$user <- FLAMINGO_GUEST_ID
  })

  observeEvent(input$abuttonloginbutton, {
    if (input$abuttonloginbutton > 0) {
      user <- isolate(input$user)
      pwd <- isolate(input$password)
      res <- session$userData$oasisapi$api_access_token(user, pwd)
      session$userData$oasisapi$set_access_token(user, pwd)
      session$userData$oasisapi$set_refresh_token(user, pwd)
      if (!is.null(session$userData$oasisapi$get_access_token())) {
        result$user <- user
        session$userData$data_hub <- DataHub$new(user =  session$userData$oasisapi$get_access_token(), destdir = getOption("flamingo.settings.api.share_filepath"))
      } else {
        result$user = FLAMINGO_GUEST_ID
        flamingoNotification("Login Failed, please check your credentials.", type = "error")
      }
      options(flamingo.settings.api.token = session$userData$oasisapi$get_access_token())
      options(flamingo.settings.api.refresh = session$userData$oasisapi$get_refresh_token())
    }
    logMessage(paste("In Login User: ", result$user))
  })

  # Module Output
  moduleOutput <- list(
    user = reactive(result$user)
  )

  return(moduleOutput)
}

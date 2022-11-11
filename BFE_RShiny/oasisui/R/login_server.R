
#' OASISUI_GUEST_ID
#'
#' @rdname OASISUI_GUEST_ID
#'
#' @description Id to use for an unidentified user.
#'
#' @return Set value.
#'
#' @export
OASISUI_GUEST_ID <- "unauthorized"

#' loginDialog
#'
#' @rdname loginDialog
#'
#' @description Server logic to login an user.
#'
#' @template params-module
#'
#' @param logout Reactive yielding logout signal.
#'
#' @return List of reactive expressions:
#' \itemize{
#' 		\item{\code{user}: }{yielding an user id if login has been completed
#' 					successfully and \link{OASISUI_GUEST_ID} otherwise}
#' 		\item{\code{logout}: }{reactive yielding logout button signal}
#' }.
#'
#' @importFrom httr content
#' @importFrom shinyjs js
#'
#' @export
loginDialog <- function(input, output, session, logout) {

  result <- reactiveValues(
    user = OASISUI_GUEST_ID
  )

  observeEvent(logout(), {
    js$reset()
    result$user <- OASISUI_GUEST_ID
    session$userData$data_hub <-  NULL
  })

  observeEvent(input$abuttonloginbutton, {
    if (input$abuttonloginbutton > 0) {
      user <- isolate(input$user)
      pwd <- isolate(input$password)
      session$userData$oasisapi$set_tokens(user, pwd)
      if (!is.null(session$userData$oasisapi$get_access_token())) {
        result$user <- user
        # initialize data_hub R6 class to manage files and files lists in OasisUI
        session$userData$data_hub <- DataHub$new(user = session$userData$oasisapi$get_access_token(), destdir = getOption("oasisui.settings.api.share_filepath"), oasisapi = session$userData$oasisapi)
      } else {
        result$user = OASISUI_GUEST_ID
        oasisuiNotification("Login Failed, please check your credentials.", type = "error")
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

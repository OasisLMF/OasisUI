
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
      # res <- api_access_token(user, pwd)
      res <- session$userData$api_hub$api_access_token(user, pwd)
      session$userData$api_hub$set_access_token(user, pwd)
      session$userData$api_hub$set_refresh_token(user, pwd)
      if (!is.null(session$userData$api_hub$get_access_tocken())){
        result$user <- user
        session$userData$data_hub <- DataHub$new(user =  session$userData$api_hub$get_access_tocken(), destdir = getOption("flamingo.settings.api.share_filepath"))
      } else {
        result$user = FLAMINGO_GUEST_ID
      }
      #remove if claise to switch on api_hub
      if (res$status == "Success") {
        result$user <- user # for later
        res <- content(res$result)
        options(flamingo.settings.api.token = res$access_token)
        options(flamingo.settings.api.refresh = res$refresh_token)
        # Data Hub
        session$userData$data_hub <- DataHub$new(user = res$access_token, destdir = getOption("flamingo.settings.api.share_filepath"))
      } else {
        options(flamingo.settings.api.token = NULL)
        flamingoNotification(type = "error",
                             "Login Failed, please check your credentials.")
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

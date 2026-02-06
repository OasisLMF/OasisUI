
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
  result <- reactiveValues(user = OASISUI_GUEST_ID)

  api <- session$userData$oasisapi
  api_auth_type <- api$get_api_auth_type()

  observeEvent(logout(), {
    if (api_auth_type != "simple") {
      shinyjs::runjs(sprintf("window.location.href='%s';", api$get_oidc_logout_url()))
    }
    js$reset()
    result$user <- OASISUI_GUEST_ID
    session$userData$data_hub <- NULL
  })

  if (api_auth_type == "simple") {
    # Simple JWT password login
    observeEvent(input$abuttonloginbutton, {
      user <- isolate(input$user)
      pwd <- isolate(input$password)
      api$set_tokens(user, pwd)
      if (!is.null(api$get_access_token())) {
        result$user <- user
        session$userData$data_hub <- DataHub$new(user = api$get_access_token(),
                                                 destdir = getOption("oasisui.settings.api.share_filepath"),
                                                 oasisapi = api)
      } else {
        result$user <- OASISUI_GUEST_ID
        oasisuiNotification("Login Failed, please check your credentials.", type = "error")
      }
    })
  } else {
    # OIDC flow
    observeEvent(input$oidc_login, {
      next_url <- session$clientData$url_pathname
      shinyjs::runjs(sprintf("window.location.href='%s';", api$get_oidc_authorize_url(next_url = next_url)))
    })

    observe({
      query <- parseQueryString(session$clientData$url_search)

      if (!is.null(query$session_token)) {
        res <- api$set_tokens_from_session(query$session_token)
        if (res) {
          username <- api$get_username_from_access_token(api$get_access_token())
          result$user <- username

          session$userData$data_hub <- DataHub$new(
            user = api$get_access_token(),
            destdir = getOption("oasisui.settings.api.share_filepath"),
            oasisapi = api
          )

          # clears tokens from URL bar
          shinyjs::runjs("history.replaceState({}, '', window.location.pathname);")
        } else {
          oasisuiNotification("OIDC token exchange failed.", type = "error")
        }
      }
    })
  }

  list(user = reactive(result$user))
}
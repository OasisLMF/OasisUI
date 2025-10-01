
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
      auth_url <- api$get_oidc_authorize_url(next_url = next_url)
      auth_url <- sub("([?&])next_url=", "\\1next=", auth_url)

      js_code <- sprintf("
        (function(){
          var popup = window.open('%s','_blank','width=900,height=700');
          if (!popup) {
            Shiny.setInputValue('oidc_error', 'popup_blocked', {priority: 'event'});
            return;
          }
          var poller = setInterval(function() {
            if (!popup || popup.closed) {
              clearInterval(poller);
              Shiny.setInputValue('oidc_error', 'popup_closed', {priority: 'event'});
              return;
            }
            try {
              var loc = popup.location.href;
              // once provider has redirected back to your domain /oidc/callback/
              if (loc.indexOf('/oidc/callback') !== -1) {
                var text = popup.document.body.innerText || popup.document.body.textContent;
                var tokens = null;
                try {
                  tokens = JSON.parse(text);
                } catch(e) {
                  console.error('Failed to parse tokens JSON', e, text);
                }
                if (tokens) {
                  Shiny.setInputValue('oidc_tokens', tokens, {priority: 'event'});
                  popup.close();
                  clearInterval(poller);
                }
              }
            } catch(e) {
              // will throw until same-origin (before redirect), just ignore
            }
          }, 500);
        })();
      ", auth_url)

      shinyjs::runjs(js_code)
    })

    # When tokens arrive from JS, set them into the OasisAPI object and set user/data_hub
    observeEvent(input$oidc_tokens, {
      tokens <- input$oidc_tokens
      # expected token keys: access_token, refresh_token, id_token (maybe)
      access <- tokens$access_token
      refresh <- tokens$refresh_token
      idtoken <- tokens$id_token

      if (is.null(access) || access == "") {
        result$user <- OASISUI_GUEST_ID
        oasisuiNotification("OIDC login failed: no access token returned", type = "error")
        return()
      }

      # set tokens into API object
      api$set_tokens_from_values(access_token = access, refresh_token = refresh)

      # try to extract a human-friendly username/email from id_token (if present)
      user_ident <- NULL
      if (!is.null(idtoken) && nzchar(idtoken)) {
        try({
          # base64url decode the middle part of JWT (payload)
          parts <- strsplit(idtoken, "\\.")[[1]]
          if (length(parts) >= 2) {
            payload_b64 <- parts[2]
            # convert base64url -> base64
            payload_b64 <- gsub('-', '+', payload_b64)
            payload_b64 <- gsub('_', '/', payload_b64)
            # pad
            pad_len <- 4 - (nchar(payload_b64) %% 4)
            if (pad_len > 0 && pad_len < 4) payload_b64 <- paste0(payload_b64, strrep('=', pad_len))
            # decode
            raw <- base64enc::base64decode(payload_b64)
            payload_json <- rawToChar(raw)
            payload <- jsonlite::fromJSON(payload_json)
            # common fields to use as user id
            if (!is.null(payload$preferred_username)) user_ident <- payload$preferred_username
            if (is.null(user_ident) && !is.null(payload$email)) user_ident <- payload$email
            if (is.null(user_ident) && !is.null(payload$name)) user_ident <- payload$name
            if (is.null(user_ident) && !is.null(payload$sub)) user_ident <- payload$sub
          }
        }, silent = TRUE)
      }

      # fallback user label if we couldn't decode id_token
      if (is.null(user_ident)) user_ident <- "OIDC_USER"

      # set UI state
      result$user <- user_ident

      # create DataHub using the access token (same behaviour as simple_jwt branch)
      session$userData$data_hub <- DataHub$new(user = api$get_access_token(),
                                               destdir = getOption("oasisui.settings.api.share_filepath"),
                                               oasisapi = api)

      oasisuiNotification(sprintf("Logged in as %s", result$user), type = "message")
    })

    # optional: handle JS-side errors (popup blocked / user closed / fetch failed)
    observeEvent(input$oidc_error, {
      err <- input$oidc_error
      if (is.null(err)) return()
      if (err == "popup_blocked") {
        oasisuiNotification("Please allow popups for this site to complete login.", type = "error")
      } else if (err == "popup_closed") {
        oasisuiNotification("Login popup was closed before completing authentication.", type = "error")
      } else {
        oasisuiNotification(paste("OIDC login error:", err), type = "error")
      }
    })
  }

  list(user = reactive(result$user))
}
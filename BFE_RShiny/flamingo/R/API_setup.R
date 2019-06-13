#' Get API env vars
#'
#' Fetches values of environment variables and combines them conveniently in a
#' list.
#'
#' @rdname APIgetenv
#'
#' @param ... Names of environment variables. If passed as named arguments, the
#'   returned list will retain the same names.
#'
#' @return List of environment variables' values.
#'
#' @export
APIgetenv <- function(...) {
  lapply(list(...), Sys.getenv)
}



#' @importFrom httr status_code
#' @importFrom httr content
api_fetch_response <- function(meth, args, logMessage = message) {
  response <- do.call(meth, eval(args, envir = sys.parent()))

  token_invalid <- status_code(response) == 401L
  # probably expired
  if (token_invalid) {
    logMessage("api: refreshing stale OAuth token")
    res <- api_refresh_token()
    if (res$status == "Success") {
      options(flamingo.settings.api.token = content(res$result)$access_token)
    } else {
      options(flamingo.settings.api.token = NULL)
    }
    response <- do.call(meth, eval(args, envir = sys.parent()))
  }
  response
}

#' @importFrom httr warn_for_status
#' @importFrom httr http_status
api_handle_response <- function(response) {
  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))

  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}

#' Get version
#'
#' @rdname get_version
#'
#' @description Gets current API version.
#'
#' @return API version.
#'
#' @export
get_version <- function() {
  getOption("flamingo.settings.api.version")
}

#' Get url
#'
#' @rdname get_url
#'
#' @description Gets current API url.
#'
#' @return URL as stored in "flamingo.settings.api" option.
#'
#' @export
get_url <- function() {
  getOption("flamingo.settings.api")$url
}

#' Get token
#'
#' @rdname get_token
#'
#' @description Gets token.
#'
#' @return Access token.
#'
#' @export
get_token <- function() {
  getOption("flamingo.settings.api.token")
}

#' Get refresh token
#'
#' Gets refresh token.
#'
#' @rdname get_refresh_token
#'
#' @return Refresh token.
#'
#' @export
get_refresh_token <- function() {
  getOption("flamingo.settings.api.refresh")
}

#' Get http type
#'
#' @rdname get_http_type
#'
#' @description Gets current http type.
#'
#' @return http type.
#'
#' @export
get_http_type <- function() {
  getOption("flamingo.settings.api.httptype")
}

#' Post refresh token
#'
#' Fetches a new access token from an existing refresh token.
#'
#' @rdname api_refresh_token
#'
#' @details Passing the `refresh_token` through the authorization header does
#'   not seem to be standard oauth2 as described in
#'   <https://tools.ietf.org/html/rfc6749>. This also makes it impossible to use
#'   `httr`'s built-in oauth2 mechanisms, which would provide automatic token
#'   refreshing within [httr::POST()], [httr::GET()], etc. Instead we have to
#'   check outside [httr::POST()] / [httr::GET()] and if necessary, refresh and
#'   redo the request. See also the unexported `api_fetch_response()`.
#'
#' @return List with API return status and response containing the new token.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#'
#' @export
#'
#' @md
api_refresh_token <- function() {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_refresh_token())
    ),
    encode = "json",
    path = "refresh_token/"
  )

  api_handle_response(response)
}


#' Perform healthcheck
#'
#' Gets the current status of the api.
#'
#' @rdname api_get_healthcheck
#'
#' @return Response containing status of API connection.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr status_code
#'
#' @export
api_get_healthcheck <- function() {

  tryCatch(
    response <- GET(
      get_url(),
      config = add_headers(
        Accept = get_http_type()
      ),
      path = "healthcheck/"
    ),
    error = function(e) {
      stop(paste("Health check failed:", e$message))
    }
  )

  if (status_code(response) != 200) {
    stop(paste("Health check failed with:", response$message))
  }

  return(status_code(response))
}

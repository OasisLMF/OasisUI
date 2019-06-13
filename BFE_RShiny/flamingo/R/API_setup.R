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
    # res <- api_refresh_token()
    # if (res$status == "Success") {
    #   options(flamingo.settings.api.token = content(res$result)$access_token)
    # } else {
      options(flamingo.settings.api.token = NULL)
    # }
    response <- do.call(meth, eval(args, envir = sys.parent()))
  }
  response
}

#' @importFrom httr warn_for_status
#' @importFrom httr http_status
api_handle_response <- function(response) {
  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) warning(w$message))

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
get_token <- function(oasisapi) {
 # oasisapi$get_access_token()
 getOption("flamingo.settings.api.token")
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

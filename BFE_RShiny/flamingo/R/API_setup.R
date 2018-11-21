#' API initialization
#'
#' Builds API URL.
#'
#' @rdname api_init
#'
#' @inheritParams flamingoServer
#'
#' @export
api_init <- function(host, port, scheme = c("http", "https")) {
  stopifnot(length(host) == 1)
  stopifnot(length(port) == 1)

  structure(
    list(
      host = host,
      port = port,
      scheme = scheme[1],
      url = paste0(scheme[1], "://", host, ":", port)
    ),
    class = c("apisettings")
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

#' Get access token
#'
#' Gets access token.
#'
#' @rdname get_access_token
#'
#' @return Access token.
#'
#' @export
get_access_token <- function() {
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
#' @rdname api_refresh_token
#'
#' @description Fetches a new refresh token from a username and password.
#'
#' @inheritParams companyDefinition
#' @inheritParams flamingoDB
#'
#' @return List with API return status and response containing the new token.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
api_refresh_token <- function(user, pwd) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type()
    ),
    body = list(username = user, password = pwd),
    encode = "json",
    path = "refresh_token/"
  )

  logWarning = warning

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

#' Post access token
#'
#' Fetches a new access token from a username and password.
#'
#' @rdname api_access_token
#'
#' @return Response containing the new token.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
api_access_token <- function() {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_access_token())
    ),
    encode = "json",
    path = "access_token/"
  )

  logWarning = warning

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

#' Perform healthcheck
#'
#' @rdname api_get_helthcheck
#'
#' @description Gets the current status of the api.
#'
#' @details Typo in he(a)lthcheck reflects typo in the API.
#'
#' @inheritParams loadProgrammeModel
#' @param ... Other inputs.
#'
#' @return Helthcheck files.
#'
#' @importFrom httr GET
#' @importFrom httr status_code
#'
#' @export
api_get_helthcheck <- function() {

  tryCatch(
    response <- GET(
      get_url(),
      config = add_headers(
        Accept = get_http_type()
      ),
      path = "helthcheck/"
    ),
    error = function(e) {
      stop(paste("He(a)lth check failed:", e$message))
    }
  )

  if (status_code(response) != 200) {
    stop(paste("He(a)lth check failed with:", response$message))
  }

  return(status_code(response))
}

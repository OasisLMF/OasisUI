### API Hub for OasisUI ----

# Description: Set of R6 classes for managing connection to API in OasisUI

# library(R6)
# library(flamingo)

### R6 Class for OasisUI API Hub ----

#' APIHub
#'
#' @rdname APIHub
#'
#' @description R6 Class for OasisUI API Hub.
#'
#' @docType class
#'
#' @return Object of \code{\link{R6Class}} with methods for connecting to Oasis API.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Arguments:
#' \describe{
#' \item{user}{string for username.}
#' \item{pwd}{string for user password.}
#' }
#'
#' @section Privates:
#' \describe{
#' \item{httptype}{Type of connection (application/json); default is NULL.}
#' \item{url}{url to connect with API; default is NULL.}
#' \item{access_token}{String for API log in; default is NULL.}
#' \item{refresh_token}{String for API access token refresh; default is NULL.}
#' \item{version}{Parameter for API connection; default is NULL.}
#' }
#'
#' @section Methods:
#' \describe{
#'
#' Initialize
#'  \item{\code{set_httptype(httptype)}}{Set httptype private.}
#'  \item{\code{api_init(host, port)}}{Initialize api and set url private.}
#'  \item{\code{set_version(version)}}{Set version private.}
#'  \item{\code{get_url()}}{Return private url.}
#'  api response
#'  \item{\code{api_handle_response(response)}}{Handles api response.}
#'  \item{\code{api_fetch_response(meth, args, logMessage = print)}}{Fetches api response.}
#'  access token
#'  \item{\code{api_access_token(user, pwd)}}{Returns response of api access token.}
#'  \item{\code{set_access_token(user, pwd)}}{Set private access token.}
#'  \item{\code{get_access_token()}}{Return private access token.}
#'  refresh token
#'  \item{\code{api_refresh_token(user, pwd)}}{Returns response of api refresh token.
#'  Passing the `refresh_token` through the authorization header does
#'   not seem to be standard oauth2 as described in
#'   <https://tools.ietf.org/html/rfc6749>. This also makes it impossible to use
#'   `httr`'s built-in oauth2 mechanisms, which would provide automatic token
#'   refreshing within [httr::POST()], [httr::GET()], etc. Instead we have to
#'   check outside [httr::POST()] / [httr::GET()] and if necessary, refresh and
#'   redo the request. See also the unexported `api_fetch_response()`.}
#'  \item{\code{set_refresh_token(user, pwd)}}{Set private refresh token.}
#'  \item{\code{get_refresh_token()}}{Return private refresh token.}
#'  version
#'  \item{\code{set_version()}}{Set private version.}
#'  \item{\code{get_version()}}{Return private version.}
#'  healtcheck
#'  \item{\code{api_get_healthcheck()}}{Perform api healthcheck.}
#'  }
#'
#' @section Usage:
#' \preformatted{api_hub <- APIHub$new()
#' api_hub$method(arg)
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom httr GET
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr status_code
#' @importFrom httr content
#'
#' @export
# APIHub ----
APIHub <- R6Class(
  "APIHub",
  # Public ----
  public = list(
    # > Initialize ----
    initialize = function(httptype = "application/json", host, port, version, ...){
      self$set_httptype(httptype)
      self$api_init(host, port)
      self$set_version(version)
    },
    set_httptype = function(httptype, ...){
      private$httptype <- httptype
    },
    api_init = function(host, port, scheme = c("http", "https"), ...) {
      stopifnot(length(host) == 1)
      stopifnot(length(port) == 1)
     conn_init <- structure(
        list(
          host = host,
          port = port,
          scheme = scheme[1],
          url = paste0(scheme[1], "://", host, ":", port)
        ),
        class = c("apisettings")
      )
      private$url <- conn_init$url
    },
    # > get url ----
    get_url = function(){
      private$url
    },
    # > api response ----
    api_handle_response = function(response, ...) {
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
    },
    api_fetch_response = function(meth, args, logMessage = print, ...) {
      response <- do.call(meth, eval(args, envir = sys.parent()))
      token_invalid <- status_code(response) == 401L
      # probably expired
      if (token_invalid) {
        logMessage("api: refreshing stale OAuth token")
        res <- self$get_refresh_token()
        if (res$status == "Success") {
          private$access_token <- content(res$result)$access_token
        } else {
          private$access_token <-  NULL
        }
        response <- do.call(meth, eval(args, envir = sys.parent()))
      }
      response
    },
    # > access token ----
    api_access_token = function(user, pwd, ...) {
      response <- POST(
        private$url,
        config = add_headers(
          Accept = private$http_type
        ),
        body = list(username = user, password = pwd),
        encode = "json",
        path = "access_token/"
      )

      self$api_handle_response(response)
    },
    set_access_token = function(user, pwd, ...){
      res <- self$api_access_token(user, pwd)
      if (res$status == "Success") {
        res <- content(res$result)
        private$access_token <- res$access_token
      } else {
        private$access_token <- NULL
      }
    },
    get_access_token = function(){
      private$access_token
    },
    # > refresh token ----
    api_refresh_token = function(...) {
      response <- POST(
        private$url,
        config = add_headers(
          Accept = private$http_type,
          Authorization = sprintf("Bearer %s", private$refresh_token)
        ),
        encode = "json",
        path = "refresh_token/"
      )
      self$api_handle_response(response)
    },
    set_refresh_token = function(user, pwd, ...){
      res <- self$api_access_token(user, pwd)
      if (res$status == "Success") {
        res <- content(res$result)
        private$refresh_token <- res$refresh_token
      } else {
        private$refresh_token <- NULL
      }
    },
    get_refresh_token = function(){
      private$refresh_token
    },
    # > version ----
    get_version = function(){
      private$version
    },
    set_version = function(version){
      private$version <- version
    },
    # > healtcheck ----
    api_get_healthcheck = function(...) {
      tryCatch(
        response <- GET(
          private$url,
          config = add_headers(
            Accept = private$http_type
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
  ),
  # Private ----
  private = list(
    httptype = NULL,
    url = NULL,
    access_token = NULL,
    refresh_token = NULL,
    version = NULL
  )
)

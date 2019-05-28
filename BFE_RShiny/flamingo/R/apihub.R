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
#' \item{yyyy}{yyyy.}
#' }
#'
#' @section Methods:
#' \describe{
#'
#' Initialize
#'  \item{\code{method(arg1,arg2)}}{dddd.}
#'}
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
    initialize = function(httptype = "application/json", host, port, version){
      self$set_httptype(httptype)
      self$api_init(host, port)
      self$set_version(version)
    },
    set_httptype = function(httptype){
      private$httptype <- httptype
    },
    api_init = function(host, port, scheme = c("http", "https")) {
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
    api_handle_response = function(response) {
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
    api_fetch_response = function(meth, args, logMessage = print) {
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
    api_access_token = function(user, pwd) {
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
    set_access_token = function(user, pwd){
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
    api_refresh_token = function() {
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
    set_refresh_token = function(user, pwd){
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
    api_get_healthcheck = function() {
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

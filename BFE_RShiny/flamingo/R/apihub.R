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
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
# APIHub ----
APIHub <- R6Class(
  "APIHub",
  # Public ----
  public = list(
    initialize = function(httptype = "application/json", host, port){
      self$set_httptype(httptype)
      self$api_init(host, port)
    },
    set_httptype = function(httptype){
      private$httptype <- httptype
    },
    api_init = function(host, port, scheme = c("http", "https")) {
      stopifnot(length(host) == 1)
      stopifnot(length(port) == 1)

      private$url <- structure(
        list(
          host = host,
          port = port,
          scheme = scheme[1],
          url = paste0(scheme[1], "://", host, ":", port)
        ),
        class = c("apisettings")
      )
    },
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
    get_access_tocken = function(){
      private$access_token
    },
    set_refresh_token = function(user, pwd){
      res <- self$api_access_token(user, pwd)
      if (res$status == "Success") {
        res <- content(res$result)
        private$refresh_token <- res$refresh_token
      } else {
        private$refresh_token <- NULL
      }
    }
  ),
  # Private ----
  private = list(
    httptype = "",
    url = "",
    access_token = NULL,
    refresh_token = NULL
  )
)

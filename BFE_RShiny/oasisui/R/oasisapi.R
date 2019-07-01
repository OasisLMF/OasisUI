### API Hub for OasisUI ----

# Description: Set of R6 classes for managing connection to API in OasisUI

# library(R6)
# library(oasisui)

### R6 Class for OasisUI API Hub ----

#' OasisAPI
#'
#' @rdname OasisAPI
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
#' @section Methods:
#' \describe{
#'
#' Initialize
#'  \item{\code{api_init(host, port)}}{Initialize api and set url private.}
#'  \item{\code{get_url()}}{Return private url.}
#'  \item{\code{get_conn_init()}}{Return conn_init.}
#'  \item{\code{get_http_type()}}{Return http_type.}
#'  api response
#'  \item{\code{api_handle_response(response)}}{Handles api response.}
#'  \item{\code{api_fetch_response(meth, args, logMessage = print)}}{Fetches api response.}
#'  access token
#'  \item{\code{api_post_access_token(user, pwd)}}{Returns response of api access token.}
#'  \item{\code{set_tokens(user, pwd)}}{Set private access token and private refresh token.}
#'  \item{\code{get_access_token()}}{Return private access token.}
#'  refresh token
#'  \item{\code{get_refresh_token()}}{Return private refresh token.}
#'  \item{\code{api_post_refresh_token()}}{Post refresh token.}
#'  version
#'  \item{\code{set_version()}}{Set private version.}
#'  \item{\code{get_version()}}{Return private version.}
#'  healtcheck
#'  \item{\code{api_get_healthcheck()}}{Perform api healthcheck.}
#'  api query
#'  \item{\code{api_query(uery_path, query_list, query_method, ...)}}{Construct query to the api.}
#'  \item{\code{api_get_query(uery_path, query_list, ...)}}{Construct GET query to the api.}
#'  \item{\code{api_post_query(uery_path, query_list, ...)}}{Construct POST query to the api.}
#'  \item{\code{api_delete_query(uery_path, query_list, ...)}}{Construct DELETE query to the api.}
#'  \item{\code{api_post_file_query(query_path, query_body = NULL, ...)}}{POST file query to the api.}
#'  \item{\code{api_body_query(query_path, query_body = NULL, query_method = "POST", ...)}}{PUT/POST query with body field to the api.}
#'  \item{\code{api_return_query_res(query_path, query_list = NULL, query_method, ...)}}{PUT/POST query with body field to the api.}
#'  return from query
#'  \item{\code{return_df(query_path, api_param = "", query_method = "GET"))}}{MAnipulate data returned from GET query.}
#'  write to disk
#'  \item{\code{api_get_analyses_tar(id, label, dest = tempfile(fileext = ".tar"))}}{GET files and write to disk tar bundle.}
#' }
#'
#' @section Usage:
#' \preformatted{oasisapi <- OasisAPI$new()
#' oasisapi$method(arg)
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom httr GET
#' @importFrom httr POST
#' @importFrom httr DELETE
#' @importFrom httr PUT
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#' @importFrom httr status_code
#' @importFrom httr content
#' @importFrom httr upload_file
#' @importFrom httr write_disk
#' @importFrom dplyr bind_rows
#'
#' @export
# OasisAPI ----
OasisAPI <- R6Class(
  "OasisAPI",
  # Private ----
  private = list(
    httptype = NULL, #Type of connection (application/json); default is NULL
    url = NULL, # url to connect with API; default is NULL
    access_token = NULL, # String for API log in; default is NULL
    refresh_token = NULL, # String for API access token refresh; default is NULL
    version = NULL, # Parameter for API connection; default is NULL
    conn_init = NULL # Structure with the api connection info; default is NULL
  ),
  # Public ----
  public = list(
    # > Initialize ----
    initialize = function(httptype = "application/json", host, port, version, ...){
      private$httptype <- httptype
      self$api_init(host, port)
      private$version <- version
    },
    get_http_type = function(){
      private$httptype
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
      private$conn_init <- conn_init
    },
    # > get conn_init ----
    get_conn_init = function(){
      private$conn_init
    },
    # > get url ----
    get_url = function(){
      private$url
    },
    # > healtcheck ----
    api_get_healthcheck = function(...) {
      tryCatch(
        response <- GET(
          private$url,
          config = add_headers(
            Accept = private$httptype
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
    },
    # > api response ----
    api_handle_response = function(response, ...) {
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
    },
    api_fetch_response = function(meth, args, logMessage = print, ...) {
      response <- do.call(meth, eval(args, envir = sys.parent()))
      token_invalid <- status_code(response) == 401L
      # probably expired
      if (token_invalid) {
        logMessage("api: refreshing stale OAuth token")
        res <- self$api_post_refresh_token()
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
    api_post_access_token = function(user, pwd, ...) {
      response <- POST(
        private$url,
        config = add_headers(
          Accept = private$httptype
        ),
        body = list(username = user, password = pwd),
        encode = "json",
        path = "access_token/"
      )

      self$api_handle_response(response)
    },
    set_tokens = function(user, pwd, ...){
      res <- self$api_post_access_token(user, pwd)
      if (res$status == "Success") {
        res <- content(res$result)
        private$access_token <- res$access_token
        private$refresh_token <- res$refresh_token
      } else {
        private$access_token <- NULL
        private$refresh_token <- NULL
      }
    },
    get_access_token = function(){
      private$access_token
    },
    # > refresh token ----
    get_refresh_token = function(){
      private$refresh_token
    },
    api_post_refresh_token = function(){
      response <- POST(
        self$get_url(),
        config = add_headers(
          Accept = self$get_http_type(),
          Authorization = sprintf("Bearer %s", self$get_refresh_token())
        ),
        encode = "json",
        path = "refresh_token/"
      )

      self$api_handle_response(response)
    },
    # > version ----
    get_version = function(){
      private$version
    },
    # > api query -----
    api_query = function(query_path, query_list = NULL, query_method, ...){

      request_list <- expression(list(
        private$url,
        config = add_headers(
          Accept = private$httptype,
          Authorization = sprintf("Bearer %s", private$access_token)
        ),
        path = paste(private$version, query_path, "", sep = "/"),
        query = query_list
      ))
      response <- self$api_fetch_response(query_method, request_list)
      self$api_handle_response(response)
    },
    api_get_query = function(query_path, query_list = NULL, ...){
      self$api_query(query_path, query_list, "GET", ...)
    },
    api_post_query = function(query_path, query_list = NULL, ...){
      self$api_query(query_path, query_list, "POST", ...)
    },
    api_delete_query = function(query_path, query_list = NULL, ...){
      self$api_query(query_path, query_list, "DELETE", ...)
    },
    api_post_file_query = function(query_path,  query_body = NULL,  ...){
      request_list <- expression(list(
        private$url,
        config = add_headers(
          Accept = private$httptype,
          Authorization = sprintf("Bearer %s", private$access_token)
        ),
        body = list(file = upload_file(query_body)),
        encode = "multipart",
        path = paste(private$version, query_path, "", sep = "/")
      ))
      response <- self$api_fetch_response("POST", request_list)
      self$api_handle_response(response)
    },
    api_body_query = function(query_path,  query_body = NULL, query_method = "POST", ...){
      request_list <- expression(list(
        private$url,
        config = add_headers(
          Accept = private$httptype,
          Authorization = sprintf("Bearer %s", private$access_token)
        ),
        body = query_body,
        encode = "json",
        path = paste(private$version, query_path, "", sep = "/")
      ))
      response <- self$api_fetch_response(query_method, request_list)
      self$api_handle_response(response)
    },
    api_return_query_res = function(query_path, query_list = NULL, query_method, ...){
      response <- self$api_query(query_path, query_list = NULL, query_method)
      content(response$result)
    },
    # > return from query ----
    return_df = function(query_path, api_param = "", query_method = "GET") {
      content_lst <- content(self$api_query(query_path, query_list = api_param,  query_method)$result)
      if (length(content_lst) > 0) {
        if (length(content_lst[[1]]) > 1) {
          content_lst <- lapply(content_lst, function(x) {lapply(x, showname)})
        } else {
          content_lst <- lapply(content_lst, showname)
        }
        if (length(content_lst) > 1 || length(content_lst[[1]]) > 1) {
          df <- bind_rows(content_lst) %>%
            as.data.frame()
        } else {
          df <- NULL
        }
      } else {
        df <- NULL
      }
      df
    },
    # > write to disk ----
    api_get_analyses_tar = function(id, label, dest = tempfile(fileext = ".tar")) {
      request_list <- expression(list(
        self$get_url(),
        config = add_headers(
          Accept = self$get_http_type(),
          Authorization = sprintf("Bearer %s",self$get_access_token())
        ),
        path = paste(self$get_version(), "analyses", id, label, "", sep = "/"),
        write_disk(dest, overwrite = TRUE)
      ))
      response <- self$api_fetch_response("GET", request_list)
      #response needed to place icon
      self$api_handle_response(response, warning)

    }
  )
)

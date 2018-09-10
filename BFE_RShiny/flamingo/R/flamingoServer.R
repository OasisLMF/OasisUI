#' Call BFE webservice to load programme data
#' @description A function to load Programme data.
#' @param apiSettings settings object as returned by [flamingoServer()]
#' @param progOasisId oasis programme identifier
#' @param logWarning warning message callback
#' @param ... additional arguments to [httr::GET()]
#' @return http status category warn_for_status
#' @importFrom httr http_status
#' @export
#' @md
loadProgrammeModel <- function(
    apiSettings,
    progOasisId,
    logWarning = function(w) warning,
    ...) {

  if (progOasisId == "") {
    stop("unexpected empty oasis programme id")
  }

  url <- modify_url(apiSettings$url, path = c("loadprogrammemodel", progOasisId))

  response <- GET(url, as = "text", ...)

  tryCatch(warn_for_status(response),
      warning = function(w) logWarning(w$message))

  return(http_status(response)$category)

}


#' Call BFE webservice to load programme data
#' @description A function to load Programme data.
#' @param apiSettings settings object as returned by [flamingoServer()]
#' @param progId programme identifier
#' @param logWarning warning message callback
#' @param ... additional arguments to [httr::GET()]
#' @return http status category warn_for_status
#' @export
#' @md
loadProgrammeData <- function(
    apiSettings,
    progId,
    logWarning = function(w) warning,
    ...) {

  if (progId == "") {
    stop("unexpected empty programme id")
  }

  url <- modify_url(apiSettings$url, path = c("loadprogrammedata", progId))

  response <- GET(url, as = "text", ...)

  tryCatch(warn_for_status(response),
      warning = function(w) logWarning(w$message))

  return(http_status(response)$category)

}


#' Create a settings object for the Flamingo API Server
#' @description creates a settings object which can then be used to connect
#' to the Flamingo API Server.
#' @param server host name
#' @param port host port
#' @param scheme communication scheme
#' @return a flamingo server settings object (HTTP by default)
#' @export
flamingoServer <- function(host, port, scheme = c("http", "https")) {

  stopifnot(length(host) == 1)
  stopifnot(length(port) == 1)

  struct <- structure(
      class = c("flamingoServer", "list"),
      list(
          host = host,
          port = port,
          scheme = scheme[1],
          url = paste0(scheme[1], "://", host, ":", port)))

  return(struct)

}

#' Flamingo API Server health check
#' @description perform a health check on the Flamingo API Server
#' @param apiSettings settings object as returned by \link{flamingoServer}
#' @param ... further arguments to \link{GET}
#' @return \code{200} if the health check succeeds; otherwise an exception is
#' raised.
#' @importFrom httr GET modify_url status_code
#' @export
testFlamingoServer <- function(apiSettings, ...) {

  url <- modify_url(apiSettings$url, path = "healthcheck")

  tryCatch(
      response <- GET(url, ...),
      error = function(e) { stop(paste("Health check failed:", e$message)) })

  if (status_code(response) != 200) {
    stop(paste("Health check failed:", response$message))
  }

  return(status_code(response))

}

#' Call BFE webservice to initiate a job
#' @description Submit a GET request to the Flamingo API Server to execute
#' the workflow.
#' @param apiSettings settings object as returned by \link{flamingoServer}
#' @param runId run identifier
#' @param logWarning warning message callback
#' @param ... further arguments to \link{GET}
#' @return http status category warn_for_status
#' @export
runProcess <- function(
    apiSettings,
    runId,
    logWarning = warning,
    ...) {

  if (runId == "") {
    stop("unexpected empty run id")
  }

  url <- modify_url(apiSettings$url, path = c("runprogoasis", runId))

  response <- GET(url, ...)

  tryCatch(warn_for_status(response),
      warning = function(w) logWarning(w$message))

  return(http_status(response)$category)

}



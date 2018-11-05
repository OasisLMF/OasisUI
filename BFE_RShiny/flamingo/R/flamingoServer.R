#' loadProgrammeModel
#'
#' @rdname loadProgrammeModel
#'
#' @description A function to load Programme data.
#'
#' @param apiSettings Settings object as returned by [flamingoServer()].
#' @param progOasisId Oasis programme identifier-
#' @param logWarning Warning message callback-
#' @param ... Additional arguments to [httr::GET()].
#'
#' @return http status category warn_for_status.
#'
#' @importFrom httr http_status
#' @importFrom httr warn_for_status
#'
#' @export
#'
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


#' loadProgrammeData
#'
#' @rdname loadProgrammeData
#'
#' @description A function to load Programme data.
#'
#' @param apiSettings Settings object as returned by [flamingoServer()].
#' @param progId programme identifier-
#' @param logWarning Warning message callback-
#' @param ... Additional arguments to [httr::GET()].
#'
#' @return http status category warn_for_status.
#'
#' @importFrom httr modify_url GET warn_for_status http_status
#'
#' @export
#'
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


#' flamingoServer
#'
#' @rdname flamingoServer
#'
#' @description Creates a settings object which can then be used to connect
#' to the Flamingo API Server.
#'
#' @param host Host name.
#' @param port Host port.
#' @param scheme Communication scheme.
#'
#' @return A flamingo server settings object (HTTP by default).
#'
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
#' testFlamingoServer
#'
#' @rdname testFlamingoServer
#'
#' @description Performs a health check on the Flamingo API Server.
#'
#' @param apiSettings as returned from \link{flamingoServer}
#' @param ... further arguments to \link{GET}.
#'
#' @return \code{200} if the health check succeeds; otherwise an exception is
#' raised.
#'
#' @importFrom httr GET modify_url
#' @importFrom httr status_code
#'
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

#' runProcess
#'
#' @rdname runProcess
#'
#' @description Submit a GET request to the Flamingo API Server to execute
#' the workflow.
#'
#' @param apiSettings as returned from \link{flamingoServer}
#' @param runId Run identifier.
#' @param logWarning Warning message callback-
#' @param ... Further arguments to \link{GET}.
#'
#' @return http status category warn_for_status.
#'
#' @importFrom httr modify_url
#' @importFrom httr GET
#' @importFrom httr warn_for_status
#'
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



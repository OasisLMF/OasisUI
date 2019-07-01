#' runOasisui
#'
#' @rdname runOasisui
#'
#' @description Runs the Oasisui Shiny application. Under normal circumstances
#' this function does not return. The application is run in a temporary
#' directory and is available under the specified port.
#'
#' @param port port to pass to \code{\link[shiny]{runApp}}.
#' @param ... Additional arguments to \code{\link[shiny]{runApp}}.
#'
#' @return No return value.
#'
#' @export
runOasisui <- function(port = 3838, ...) {

  runApp(appDir = system.file("app", package = "oasisui"), port = port, ...)

}

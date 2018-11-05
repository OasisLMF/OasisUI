#' runFlamingo
#'
#' @rdname runFlamingo
#'
#' @description Runs the Flamingo Shiny application. Under normal circumstances
#' this function does not return. The application is run in a temporary
#' directory and is available under the specified port.
#'
#' @param ... Additional arguments to \code{\link[shiny]{runApp}}.
#'
#' @return No return value.
#'
#' @export
runFlamingo <- function(port = 3838, ...) {

  runApp(appDir = system.file("app", package = "flamingo"), port = port, ...)

}

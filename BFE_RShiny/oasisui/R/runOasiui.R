#' runOasisui
#'
#' @rdname runOasisui
#'
#' @description Runs the Oasisui Shiny application. Under normal circumstances
#' this function does not return. The application is run in a temporary
#' directory and is available under the specified port. The Sys Env APP_DIR
#' determines which sub-directory of `BFE_RShiny/oasisui` to run the Shiny
#' App from. Currently, the available options are "app" (full application) and
#' "appli" (minified application for public access).
#' 
#' @param port port to pass to \code{\link[shiny]{runApp}}.
#' @param ... Additional arguments to \code{\link[shiny]{runApp}}.
#'
#' @return No return value.
#'
#' @export
runOasisui <- function(port = 3838, ...) {
  # Read the APP_DIR environment variable
  app_dir <- Sys.getenv("APP_DIR", unset = "app")
  
  # Validate the app's working directory
  if (!app_dir %in% c("app", "appli")) {
    stop(sprintf("Invalid APP_DIR environment variable: '%s'. Expected 'app' or 'appli'.", app_dir))
  }
  
  # Run the Shiny App from the specified app directory
  runApp(appDir = system.file(app_dir, package = "oasisui"), port = port, ...)
}

###############################################################################
# note with shiny::runApp this statement isn't needed anymore
# library(shiny, warn.conflicts = FALSE, quietly = TRUE)

library(flamingo, warn.conflicts = FALSE)

source(file.path(".", "helper_text.R"), local = TRUE)

### logger ---------------------------------------------------------------------
#addHandler(writeToFile, logger = "flamingo",
#    file = file.path("var", "log", "shinyproxy", "flamingo.log"))

#addHandler(writeToConsole, logger = "flamingo")

#' loginfo
#'
#' @rdname loginfo
#'
#' @param ... Extra arguments.
#' @param logger flamingo.module.
#'
#' @return Log in info.
#'
#' @export
loginfo <- function(..., logger) {message(...)}

#' logerror
#'
#' @rdname logerror
#'
#' @param ... Extra arguments.
#' @param logger flamingo.module.
#'
#' @return Warning message.
#'
#' @export
logerror <- function(..., logger) {warning(...)}

loginfo("testing logger", logger = "flamingo.module")

logMessage <- function(msg) loginfo(msg, logger = "flamingo.module")

### Django API -----------------------------------------------------------------
APISettings <- APIgetenv(
  server = "API_IP",
  port = "API_PORT",
  version = "API_VERSION",
  share_filepath = "API_SHARE_FILEPATH"
)

# options(flamingo.settings.api = api_init("localhost", "8000"))
options(flamingo.settings.api = api_init(APISettings$server, APISettings$port))
options(flamingo.settings.api.httptype = "application/json")
options(flamingo.settings.api.version = APISettings$version)
options(flamingo.settings.api.share_filepath = APISettings$share_filepath)

options(flamingo.settings.oasis_environment = Sys.getenv("OASIS_ENVIRONMENT"))

#health check
loginfo(paste("flamingo API server:", get_url()), logger = "flamingo.module")
tryCatch({
  invisible(api_get_healthcheck())
}, error = function(e) {
  logerror(e$message, logger = "flamingo.module")
})

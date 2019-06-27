###############################################################################
# note with shiny::runApp this statement isn't needed anymore
# library(shiny, warn.conflicts = FALSE, quietly = TRUE)

library(oasisui, warn.conflicts = FALSE)

source(file.path(".", "helper_text.R"), local = TRUE)

#Extend max uploadable file size from default 5MB to 40MB
options(shiny.maxRequestSize = 400*1024^2)

### logger ---------------------------------------------------------------------
#addHandler(writeToFile, logger = "oasisui",
#    file = file.path("var", "log", "shinyproxy", "oasisui.log"))

#addHandler(writeToConsole, logger = "oasisui")

#' loginfo
#'
#' @rdname loginfo
#'
#' @param ... Extra arguments.
#' @param logger oasisui.module.
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
#' @param logger oasisui.module.
#'
#' @return Warning message.
#'
#' @export
logerror <- function(..., logger) {warning(...)}

loginfo("testing logger", logger = "oasisui.module")

logMessage <- function(msg) loginfo(msg, logger = "oasisui.module")

### Django API -----------------------------------------------------------------
APISettings <- APIgetenv(
  server = "API_IP",
  port = "API_PORT",
  version = "API_VERSION",
  share_filepath = "API_SHARE_FILEPATH"
)

# options(oasisui.settings.api = api_init("localhost", "8000"))
# options(oasisui.settings.api = api_init(APISettings$server, APISettings$port))
options(oasisui.settings.api.httptype = "application/json")
options(oasisui.settings.api.version = APISettings$version)
options(oasisui.settings.api.share_filepath = APISettings$share_filepath)

options(oasisui.settings.admin.mode = Sys.getenv("ADMIN_MODE"))

options(oasisui.settings.oasis_environment = Sys.getenv("OASIS_ENVIRONMENT"))

###############################################################################
# note with shiny::runApp this statement isn't needed anymore
# library(shiny, warn.conflicts = FALSE, quietly = TRUE)

library(flamingo, warn.conflicts = FALSE)

source(file.path(".", "helper_text.R"), local = TRUE)

#Extend max uploadable file size from default 5MB to 40MB
options(shiny.maxRequestSize = 400*1024^2)

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
options(flamingo.settings.api.server = Sys.getenv("API_IP"))
options(flamingo.settings.api.port = Sys.getenv("API_PORT"))
options(flamingo.settings.api.httptype = "application/json")
options(flamingo.settings.api.version = Sys.getenv("API_VERSION"))
options(flamingo.settings.api.share_filepath = Sys.getenv("API_SHARE_FILEPATH"))

options(flamingo.settings.admin.mode = Sys.getenv("ADMIN_MODE"))

options(flamingo.settings.oasis_environment = Sys.getenv("OASIS_ENVIRONMENT"))

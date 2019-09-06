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


#' @rdname loginfo
#'
#' @param msg Character string message.
#'
#' @export
logMessage <- function(msg) loginfo(msg, logger = "oasisui.module")

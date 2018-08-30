
#' Flamingo Module Template UI
#' @param id module id
flamingoModuleUI <- function(
    id) {}


#' Flamingo Module Template
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param active reactive expression whether the module state should be updated.
#' @param dbSettings as returned from \link{flamingoDB}
#' @param apiSettings as returned from \link{flamingoServer}
#' @param logMessage function that will be passed info messages
#' @param logError function that will be passed error messages
flamingoModule <- function(
    input,
    output,
    session,
    active,
    dbSettings,
    apiSettings,
    logMessage,
    logError) {}


#' HTML Dependencies For The Flamingo Interface
#' @importFrom htmltools htmlDependency
#' @export
flamingoHtmlDependencies <- function() {
  
  htmlDependency(name = "flamingo", version = "0.0.4", src = c(href = "assets"),
      stylesheet = c("css/flamingo-tweaks.css", "css/flamingo-table.css"))
  
}


#' Action button/link
#' @description Modified version of the default \link{shiny::actionButton}
#' @param class HTML class attribute
#' @param ... arguments to \link{shiny::actionButton}
#' @export
actionButton <- function(class = c("btn", "btn-primary", "btn-flamingo"), ...) {
  shiny::actionButton(class = class, ...)
}


#' Show or remove a notification
#' @description Modified version of the default \link{shiny::showNotifcation}
#' @param ui see \link{shiny::showNotification}
#' @param type see \link{shiny::showNotification}
#' @param ... other arguments to \link{shiny::showNotification}
#' @export
showNotification <- function(ui,
    type = c("default", "message", "warning", "error"), ...) {
  
  iconName <- switch(type, "warning" = "exclamation-triangle",
      "message" = "check-circle", "error" = "minus-circle", "info-circle")
  
  shiny::showNotification(
      ui = tagList(
          div(class = "shiny-notification-side", icon(iconName)),
          div(class = "shiny-notification-main", ui)),
      type = type,
      ...)
}


#' Create Choices For Select Input Widgets
#' @description Converts a table into a named list of choices for use in
#' \link{selectInput}.
#' @param df \code{data.frame} as returned by e.g. \link{getCompanyList}
#' @param label label for extra option to add at the top. If \code{NA} (default)
#' no extra option will be added to the top.
#' @param value value for extra option to add at the top
#' @param labelCol column index of the column that is used for the labels
#' @param valueCol column index of the column that is used for the values
#' @importFrom stats setNames
#' @export
createSelectOptions <- function(df, label = NA, value = "0", labelCol = 2,
    valueCol = 1) {
  if (!is.null(df) && nrow(df) > 0) {
    if (!is.na(label)) {
      selectOptions <- setNames(list(value), label)
    } else {
      selectOptions <- list()
    }
    selectOptions[as.character(df[,labelCol])] <- as.character(df[,valueCol])
    return(selectOptions)
  }
}
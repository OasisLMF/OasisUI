#' flamingoButton
#'
#' @rdname flamingoButton
#'
#' @description Modified version of the default [shiny::actionButton()].
#'
#' @param class HTML class attribute.
#' @param inputId widget id
#' @param label widget label 
#' @param icon widget icon
#' @param width widget width
#' @param ... Arguments to [shiny::actionButton()].
#'
#' @return Acces to Oasis UI.
#'
#' @export
#'
#' @md
flamingoButton <- function(inputId, label, icon = NULL, width = NULL, class = c("btn", "btn-primary"), ...) {
  value <- restoreInput(id = inputId, default = NULL)
  df_class <- c("btn", "btn-default", "action-button")
  fl_class <- paste(union(class, df_class), collapse = " ")
  tags$button(
    id = inputId,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    type = "button",
    class = fl_class,
    `data-val` = value,
    list(shiny:::validateIcon(icon), label),
    ...
  )
}

#' flamingoCheckboxButton
#'
#' @rdname flamingoCheckboxButton
#'
#' @description Modified version of the default [shiny::actionButton()].
#'
#' @param inputId widget id
#' @param label widget label 
#' @param icon widget icon
#' @param width widget width
#' @param class HTML class attribute.
#' @param ... Arguments to [shiny::actionButton()].
#'
#' @return List of tags.
#'
#' @export
#'
#' @md
flamingoCheckboxButton <- function(inputId, label, icon = NULL, width = NULL, class = c("btn"), ...) {
  value <- restoreInput(id = inputId, default = NULL)
  df_class <- c("btn", "action-button")
  fl_class <- paste(union(class, df_class), collapse = " ")
  tags$button(
    id = inputId,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    type = "button",
    class = fl_class,
    `data-val` = value,
    list(shiny:::validateIcon(icon), label),
    ...
  )
}


#' flamingoNotification
#'
#' @rdname flamingoNotification
#'
#' @description Modified version of the default [shiny::showNotification()].
#'
#' @param ui Content of message.
#' @param type see [shiny::showNotification()].
#' @param ... other arguments to [shiny::showNotification()].
#'
#' @return Notifications.
#'
#' @export
#'
#' @md
flamingoNotification <- function(ui, type = c("default", "message", "warning", "error"), ...) {

  iconName <- switch(type,
                     "warning" = "exclamation-triangle",
                     "message" = "check-circle",
                     "error" = "minus-circle",
                     "info-circle")

  invisible(showNotification(
    ui = tagList(
      div(class = "shiny-notification-side", icon(iconName)),
      div(class = "shiny-notification-main", ui)
    ),
    type = type,
    ...
  ))
}



#' createSelectOptions
#'
#' @rdname createSelectOptions
#'
#' @description Converts a table into a named list of choices for use in
#' \link{selectInput}.
#'
#' @param df \code{data.frame}.
#' @param label Label for extra option to add at the top. If \code{NA} (default)
#' no extra option will be added to the top.
#' @param value value for extra option to add at the top.
#' @param labelCol Column index of the column that is used for the labels.
#' @param valueCol Column index of the column that is used for the values.
#'
#' @return smth.
#'
#' @importFrom stats setNames
#'
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

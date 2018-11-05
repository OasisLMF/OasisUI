
#' flamingoModuleUI
#'
#' @rdname flamingoModuleUI
#'
#' @description Flamingo Module Template UI.
#'
#' @inheritParams accountDefinitionUI
#'
#' @return No value.
flamingoModuleUI <- function(id) {}


#' Flamingo Module Template
#'
#' @rdname flamingoModule
#'
#' @inheritParams loadProgrammeModel
#' @inheritParams flamingoDBLogin
#' @inheritParams executeDbQuery
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param active Reactive expression whether the module state should be updated.
#' @param logMessage Function that will be passed info messages.
#' @param logError Function that will be passed error messages.
#'
#' @export
flamingoModule <- function(
  input,
  output,
  session,
  active,
  dbSettings,
  apiSettings,
  logMessage,
  logError) {}


#' flamingoButton
#'
#' @rdname flamingoButton
#'
#' @description Modified version of the default [shiny::actionButton()].
#'
#' @param class HTML class attribute.
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
#' @inheritParams flamingoButton
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
#' @inheritParams companyDefinition
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
#' @inheritParams replaceWithIcons
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

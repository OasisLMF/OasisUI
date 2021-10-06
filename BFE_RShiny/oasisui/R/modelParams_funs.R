#' Basic Output Configuration functions
#'
#' @description Functions for the Basic Output Configuration/Rerun Model Parameters view.
#'
#' @param session Current session.
#' @param model_settings Model settings retrieved from the API.
#'
#' @return List of UI elements (input/selector widgets).
#'
#' @export
basicConfig_funs <- function(session, model_settings) {
  ns <- session$ns
  # Event set
  .event_set_fun <- function(model_settings) {
    selectChoices <- lapply(model_settings$event_set.options, function(x) {
      x$id
    })
    names(selectChoices) <- sapply(model_settings$event_set.options, function(x) {
      x$desc
    })
    doHide <- length(model_settings$event_set.options) < 2
    style <- if (doHide) "display: none" else ""
    selectInput(
      inputId = ns("event_set"),
      label = "Event Set:",
      choices = selectChoices,
      selected = model_settings$event_set.default,
      multiple = FALSE
    ) %>% tagAppendAttributes(id = ns("eventset_ctnr"), style = style)
  }

  # Event occurrence
  .event_occurrence_fun <- function(model_settings) {
    selectChoices <- lapply(model_settings$event_occurrence_id.options, function(x) {
      x$id
    })
    names(selectChoices) <- sapply(model_settings$event_occurrence_id.options, function(x) {
      x$desc
    })
    doHide <- length(model_settings$event_occurrence_id.options) < 2
    style <- if (doHide) "display: none" else ""
    selectInput(
      inputId = ns("event_occurrence"),
      label = "Event Occurrence:",
      choices = selectChoices,
      selected = model_settings$event_occurrence_id.default,
      multiple = FALSE
    ) %>% tagAppendAttributes(id = ns("eventoccurrence_ctnr"), style = style)
  }

  tagList(.event_set_fun(model_settings),
          .event_occurrence_fun(model_settings)
  )
}

#' Advanced Output Configuration functions
#'
#' @description Functions for the Advanced Output Configuration/Rerun Model Parameters view.
#'
#' @param session Current session.
#' @param model_settings Model settings retrieved from the API.
#'
#' @importFrom shinyBS bsPopover
#'
#' @return List of UI elements (input/selector widgets).
#'
#' @export
advancedConfig_funs <- function(session, model_settings) {
  ns <- session$ns

  # string parameters
  .string_fun <- function(model_settings) {
    if (length(grep("string_parameters", names(model_settings))) > 0) {
      lapply(grep("string_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$tooltip)) {
          tooltip_tex <- model_settings[[x]]$tooltip
        } else {
          tooltip_tex <- model_settings[[x]]$name
        }
        if (!is.null(model_settings[[x]]$desc)) {
          label_widget <- model_settings[[x]]$desc
        } else {
          label_widget <- gsub("_", " ", model_settings[[x]]$name)
        }
        if (is.null(model_settings[[x]]$used_for) || model_settings[[x]]$used_for == "losses") {
          fluidRow(
            column(10,
                   textInput(
                     inputId = ns(paste0("string_parameters", x)),
                     label = div(paste0(label_widget, ":") %>% capitalize_first_letter()),
                     value = model_settings[[x]]$default
                   )),
            column(1,
                   actionButton(paste0("tooltip_string_", x), "", icon = icon("info"), style='padding:4px; font-size:80%'),
                   bsPopover(id = paste0("tooltip_string_", x), title = "",
                             content = tooltip_tex,
                             placement = "right",
                             trigger = "focus",
                             options = list(container = "body"))))
        }
      })
    }
  }

  # list parameters
  .list_fun <- function(model_settings) {
    if (length(grep("list_parameters", names(model_settings))) > 0) {
      lapply(grep("list_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$tooltip)) {
          tooltip_tex <- model_settings[[x]]$tooltip
        } else {
          tooltip_tex <- model_settings[[x]]$name
        }
        if (!is.null(model_settings[[x]]$desc)) {
          label_widget <- model_settings[[x]]$desc
        } else {
          label_widget <- gsub("_", " ", model_settings[[x]]$name)
        }
        if (is.null(model_settings[[x]]$used_for) || model_settings[[x]]$used_for == "losses") {
          fluidRow(
            column(10,
                   textInput(
                     inputId = ns(paste0("list_parameters", x)),
                     label = paste0(label_widget, ":") %>% capitalize_first_letter(),
                     value = paste(unlist(model_settings[[x]]$default), collapse = ", ")
                   )),
            column(1,
                   actionButton(paste0("tooltip_list_", x), "", icon = icon("info"), style='padding:4px; font-size:80%'),
                   bsPopover(id = paste0("tooltip_list_", x), title = "",
                             content = tooltip_tex,
                             placement = "right",
                             trigger = "focus",
                             options = list(container = "body")))
          )
        }
      })
    }
  }

  # dictionary parameters
  .dictionary_fun <- function(model_settings) {
    if (length(grep("dictionary_parameters", names(model_settings))) > 0) {
      lapply(grep("dictionary_parameters", names(model_settings)), function(x) {
        if (is.null(model_settings[[x]]$used_for) || model_settings[[x]]$used_for == "losses") {
          if (!is.null(model_settings[[x]]$tooltip)) {
            tooltip_tex <- model_settings[[x]]$tooltip
          } else {
            tooltip_tex <- model_settings[[x]]$name
          }
          if (!is.null(model_settings[[x]]$desc)) {
            label_widget <- model_settings[[x]]$desc
          } else {
            label_widget <- model_settings[[x]]$name
          }
          lapply(seq_len(length(model_settings[[x]]$default)), function(y) {
            fluidRow(
              column(10,
                     textInput(
                       inputId = ns(paste0("dictionary_parameters", x, y)),
                       label = paste(model_settings[[x]]$name, names(model_settings[[x]]$default[y]), sep = ": "),
                       value = model_settings[[x]]$default[[y]]
                     )),
              column(
                1,actionButton(paste0("tooltip_dict_", x), "", icon = icon("info"), style='padding:4px; font-size:80%'),
                bsPopover(id = paste0("tooltip_dict_", x), title = "",
                          content = tooltip_tex,
                          placement = "right",
                          trigger = "focus",
                          options = list(container = "body")))
            )
          })
        }
      })
    }
  }

  # boolean parameters
  .boolean_params_fun <- function(model_settings) {
    if (length(grep("boolean_parameters", names(model_settings))) > 0) {
      lapply(grep("boolean_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$tooltip)) {
          tooltip_tex <- model_settings[[x]]$tooltip
        } else {
          tooltip_tex <- model_settings[[x]]$name
        }
        if (!is.null(model_settings[[x]]$desc)) {
          label_widget <- model_settings[[x]]$desc
        } else {
          label_widget <- gsub("_", " ", model_settings[[x]]$name)
        }
        if (is.null(model_settings[[x]]$used_for) || model_settings[[x]]$used_for == "losses") {
          fluidRow(
            column(10,
                   checkboxInput(
                     inputId = ns(paste0("boolean_parameters", x)),
                     label = label_widget %>% capitalize_first_letter(),
                     value = model_settings[[x]]$default
                   )),
            column(1,
                   actionButton(paste0("tooltip_bool_", x), "", icon = icon("info"), style='padding:4px; font-size:80%'),
                   bsPopover(id = paste0("tooltip_bool_", x), title = "",
                             content = tooltip_tex,
                             placement = "right",
                             trigger = "focus",
                             options = list(container = "body")))
          )
        }
      })
    }
  }

  # float parameters
  .float_fun <- function(model_settings) {
    if (length(grep("float_parameters", names(model_settings))) > 0) {
      lapply(grep("float_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$tooltip)) {
          tooltip_tex <- model_settings[[x]]$tooltip
        } else {
          tooltip_tex <- model_settings[[x]]$name
        }
        if (!is.null(model_settings[[x]]$desc)) {
          label_widget <- model_settings[[x]]$desc
        } else {
          label_widget <- gsub("_", " ", model_settings[[x]]$name)
        }
        if (is.null(model_settings[[x]]$used_for) || model_settings[[x]]$used_for == "losses") {
          fluidRow(
            column(10,
                   sliderInput(
                     inputId = ns(paste0("float_parameters", x)),
                     label = paste0(label_widget, ":") %>% capitalize_first_letter(),
                     min = model_settings[[x]]$min,
                     max = model_settings[[x]]$max,
                     value = model_settings[[x]]$default
                   )),
            column(
              1,actionButton(paste0("tooltip_float_", x), "", icon = icon("info"), style='padding:4px; font-size:80%'),
              bsPopover(id = paste0("tooltip_float_", x), title = "",
                        content = tooltip_tex,
                        placement = "right",
                        trigger = "focus",
                        options = list(container = "body")))
          )
        }
      })
    }
  }

  # drop-down parameters
  .dropdown_fun <- function(model_settings) {
    if (length(grep("dropdown_parameters", names(model_settings))) > 0) {
      lapply(grep("dropdown_parameters", names(model_settings)), function(x) {
        if (!is.null(model_settings[[x]]$tooltip)) {
          tooltip_tex <- model_settings[[x]]$tooltip
        } else {
          tooltip_tex <- model_settings[[x]]$name
        }
        if (!is.null(model_settings[[x]]$desc)) {
          label_widget <- model_settings[[x]]$desc
        } else {
          label_widget <- gsub("_", " ", model_settings[[x]]$name)
        }
        list_values <- lapply(seq_len(length(model_settings[[x]]$options)), function (y) {
          model_settings[[x]]$options[[y]]$id
        })
        if (is.null(model_settings[[x]]$used_for) || model_settings[[x]]$used_for == "losses") {
          fluidRow(
            column(10,
                   selectInput(
                     inputId = ns(paste0("dropdown_parameters", x)),
                     label = paste0(label_widget, sep = ": "),
                     choices = list_values,
                     selected = model_settings[[x]]$default
                   )),
            column(1,
                   actionButton(paste0("tooltip_dropdown_", x), "", icon = icon("info"), style='padding:4px; font-size:80%'),
                   bsPopover(id = paste0("tooltip_dropdown_", x), title = "",
                             content = tooltip_tex,
                             placement = "right",
                             trigger = "focus",
                             options = list(container = "body")))
          )
        }
      })
    }
  }

  # # supported perils parameters
  # .perils_fun <- function(tbl_modelsDetails, model_settings) {
  #   if (length(grep("supported_perils", names(model_settings))) > 0) {
  #     selectInput(
  #       inputId = ns("supported_perils"),
  #       label = "Supported Perils:",
  #       choices = lapply(seq(1, length(tbl_modelsDetails$lookup_settings$supported_perils)), function (y) {
  #         tbl_modelsDetails$lookup_settings$supported_perils[[y]]$id}),
  #       selected = lapply(seq(1, length(tbl_modelsDetails$lookup_settings$supported_perils)), function (y) {
  #         tbl_modelsDetails$lookup_settings$supported_perils[[y]]$id}),
  #       multiple = TRUE
  #     )
  #   }
  # }

  tagList(.string_fun(model_settings),
          .list_fun(model_settings),
          .dictionary_fun(model_settings),
          .boolean_params_fun(model_settings),
          .float_fun(model_settings),
          .dropdown_fun(model_settings))
}

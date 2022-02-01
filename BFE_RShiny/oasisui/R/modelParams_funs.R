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
    if (is.null(model_settings$event_set.used_for) || model_settings$event_set.used_for == "losses") {
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
      ) %>% tagAppendAttributes(id = ns("eventset_ctnr"))
    }
  }

  # Event occurrence
  .event_occurrence_fun <- function(model_settings) {
    if (is.null(model_settings$event_occurrence_id.used_for) || model_settings$event_occurrence_id.used_for == "losses") {
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
      ) %>% tagAppendAttributes(id = ns("eventoccurrence_ctnr"))
    }
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
#' @importFrom bsplus bs_embed_popover
#' @importFrom bsplus use_bs_popover
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
          fluidRow(use_bs_popover(),
                   column(10,
                          textInput(
                            inputId = ns(paste0("string_parameters", x)),
                            label = div(paste0(label_widget, ":") %>% capitalize_first_letter()),
                            value = model_settings[[x]]$default
                          )),
                   column(1,
                          actionButton(ns(paste0("tooltip_string_", x)), "", icon = icon("info"),
                                       style='padding:4px; font-size:80%') %>%
                            bs_embed_popover(
                              title = NULL,
                              content = tooltip_tex
                            )
                   )
          )
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
          fluidRow(use_bs_popover(),
                   column(10,
                          textInput(
                            inputId = ns(paste0("list_parameters", x)),
                            label = paste0(label_widget, ":") %>% capitalize_first_letter(),
                            value = paste(unlist(model_settings[[x]]$default), collapse = ", ")
                          )),
                   column(1,
                          actionButton(ns(paste0("tooltip_list_", x)), "", icon = icon("info"),
                                       style='padding:4px; font-size:80%') %>%
                            bs_embed_popover(
                              title = NULL,
                              content = tooltip_tex
                            )
                   )
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
          lapply(seq_len(length(model_settings[[x]]$default)), function(y) {
            fluidRow(use_bs_popover(),
                     column(10,
                            textInput(
                              inputId = ns(paste0("dictionary_parameters", x, y)),
                              label = paste(model_settings[[x]]$name, names(model_settings[[x]]$default[y]), sep = ": "),
                              value = model_settings[[x]]$default[[y]]
                            )),
                     column(1,
                            actionButton(ns(paste0("tooltip_dict_", x)), "", icon = icon("info"),
                                         style='padding:4px; font-size:80%') %>%
                              bs_embed_popover(
                                title = NULL,
                                content = tooltip_tex
                              )
                     )
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
          fluidRow(use_bs_popover(),
                   column(10,
                          checkboxInput(
                            inputId = ns(paste0("boolean_parameters", x)),
                            label = label_widget %>% capitalize_first_letter(),
                            value = model_settings[[x]]$default
                          )),
                   column(1,
                          actionButton(ns(paste0("tooltip_bool_", x)), "", icon = icon("info"),
                                       style='padding:4px; font-size:80%')  %>%
                            bs_embed_popover(
                              title = NULL,
                              content = tooltip_tex
                            )
                   )
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
        if (!is.null(model_settings[[x]]$stepsize)) {
          step_size <- model_settings[[x]]$stepsize
        } else {
          step_size <- model_settings[[x]]$default
        }
        if (is.null(model_settings[[x]]$used_for) || model_settings[[x]]$used_for == "losses") {
          fluidRow(use_bs_popover(),
                   column(10,
                          sliderInput(
                            inputId = ns(paste0("float_parameters", x)),
                            label = paste0(label_widget, ":") %>% capitalize_first_letter(),
                            min = model_settings[[x]]$min,
                            max = model_settings[[x]]$max,
                            step = step_size,
                            value = model_settings[[x]]$default
                          )
                   ),
                   column(1,
                          actionButton(ns(paste0("tooltip_float_", x)), "", icon = icon("info"),
                                       style='padding:4px; font-size:80%') %>%
                            bs_embed_popover(
                              title = NULL,
                              content = tooltip_tex
                            )
                   )
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
          fluidRow(use_bs_popover(),
                   column(10,
                          selectInput(
                            inputId = ns(paste0("dropdown_parameters", x)),
                            label = paste0(label_widget, sep = ": "),
                            choices = list_values,
                            selected = model_settings[[x]]$default,
                            multiple = TRUE
                          )),
                   column(1,
                          actionButton(ns(paste0("tooltip_dropdown_", x)), "", icon = icon("info"),
                                       style='padding:4px; font-size:80%') %>%
                            bs_embed_popover(
                              title = NULL,
                              content = tooltip_tex
                            )
                   )
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


#' Global Model Settings parameters function
#'
#' @description Functions for the Build Custom UI.
#'
#' @param session Current session.
#' @param model_settings Model settings retrieved from the API.
#' @param ui_step Step 2.
#' @param ls_param_group Parameter groups.
#'
#' @importFrom bsplus bs_embed_popover
#' @importFrom bsplus use_bs_popover
#'
#' @return List of UI elements (input/selector widgets).
#'
#' @export
Global_funs <- function(session, model_settings, ui_step, ls_param_group) {
  ns <- session$ns

  # account for event_set and event_occurrence_id which are outside of the other parameters
  .event_set_fun_g <- function(model_settings) {
    if (length(grep("event_set", ls_param_group)) > 0) {
      if (is.null(model_settings$event_set$used_for) || model_settings$event_set$used_for == ui_step) {
        selectChoices <- lapply(model_settings$event_set$options, function(x) {
          x$id
        })
        names(selectChoices) <- sapply(model_settings$event_set$options, function(x) {
          x$desc
        })
        doHide <- length(model_settings$event_set$options) < 2
        style <- if (doHide) "display: none" else ""
        selectInput(
          inputId = ns("event_set_g"),
          label = "Event Set:",
          choices = selectChoices,
          selected = model_settings$event_set$default,
          multiple = FALSE
        ) %>% tagAppendAttributes(id = ns("eventset_ctnr"), style = style)
      }
    }
  }
  .event_occurrence_fun_g <- function(model_settings) {
    if (length(grep("event_occurrence_id", ls_param_group)) > 0) {
      if (is.null(model_settings$event_occurrence_id$used_for) || model_settings$event_occurrence_id$used_for == ui_step) {
        selectChoices <- lapply(model_settings$event_occurrence_id$options, function(x) {
          x$id
        })
        names(selectChoices) <- sapply(model_settings$event_occurrence_id$options, function(x) {
          x$desc
        })
        doHide <- length(model_settings$event_occurrence_id$options) < 2
        style <- if (doHide) "display: none" else ""
        selectInput(
          inputId = ns("event_occurrence_g"),
          label = "Event Occurrence:",
          choices = selectChoices,
          selected = model_settings$event_occurrence_id$default,
          multiple = FALSE
        ) %>% tagAppendAttributes(id = ns("eventoccurrence_ctnr"), style = style)
      }
    }
  }

  # string parameters
  .string_fun <- function(model_settings) {
    if (length(grep("string_parameters", names(model_settings))) > 0) {
      x <- grep("string_parameters", names(model_settings))
      lapply(seq_len(length(model_settings[[x]])), function(y) {
        if (length(grep(model_settings[[x]][[y]]$name, ls_param_group)) > 0) {
          if (!is.null(model_settings[[x]][[y]]$tooltip)) {
            tooltip_tex <- model_settings[[x]][[y]]$tooltip
            # limit number of characters in tooltip box
            if (nchar(tooltip_tex) > 100) {
              tooltip_tex <- paste0(substr(tooltip_tex, start = 1, stop = 100), "...")
            }
          } else {
            tooltip_tex <- model_settings[[x]][[y]]$name
          }
          if (!is.null(model_settings[[x]][[y]]$desc)) {
            label_widget <- model_settings[[x]][[y]]$desc
          } else {
            label_widget <- gsub("_", " ", model_settings[[x]][[y]]$name)
          }
          if (is.null(model_settings[[x]][[y]]$used_for) || model_settings[[x]][[y]]$used_for == ui_step) {
            fluidRow(use_bs_popover(),
                     column(4,
                            textInput(
                              inputId = ns(paste0("string_parameters", y)),
                              label = div(paste0(label_widget, ":") %>% capitalize_first_letter()),
                              value = model_settings[[x]][[y]]$default
                            )),
                     column(1,
                            actionButton(ns(paste0("tooltip_string_", y)), "", icon = icon("info"),
                                         style='padding:4px; font-size:80%') %>%
                              bs_embed_popover(
                                title = NULL,
                                content = tooltip_tex
                              )
                     )
            )
          }
        }
      })
    }
  }

  # list parameters
  .list_fun <- function(model_settings) {
    if (length(grep("list_parameters", names(model_settings))) > 0) {
      x <- grep("list_parameters", names(model_settings))
      lapply(seq_len(length(model_settings[[x]])), function(y) {
        if (length(grep(model_settings[[x]][[y]]$name, ls_param_group)) > 0) {
          if (!is.null(model_settings[[x]][[y]]$tooltip)) {
            tooltip_tex <- model_settings[[x]][[y]]$tooltip
            # limit number of characters in tooltip box
            if (nchar(tooltip_tex) > 100) {
              tooltip_tex <- paste0(substr(tooltip_tex, start = 1, stop = 100), "...")
            }
          } else {
            tooltip_tex <- model_settings[[x]][[y]]$name
          }
          if (!is.null(model_settings[[x]][[y]]$desc)) {
            label_widget <- model_settings[[x]][[y]]$desc
          } else {
            label_widget <- gsub("_", " ", model_settings[[x]][[y]]$name)
          }
          if (is.null(model_settings[[x]][[y]]$used_for) || model_settings[[x]][[y]]$used_for == ui_step) {
            fluidRow(use_bs_popover(),
                     column(4,
                            textInput(
                              inputId = ns(paste0("list_parameters", y)),
                              label = paste0(label_widget, ":") %>% capitalize_first_letter(),
                              value = paste(unlist(model_settings[[x]][[y]]$default), collapse = " , ")
                            )),
                     column(1,
                            actionButton(ns(paste0("tooltip_list_", y)), "", icon = icon("info"),
                                         style='padding:4px; font-size:80%') %>%
                              bs_embed_popover(
                                title = NULL,
                                content = tooltip_tex
                              )
                     )
            )
          }
        }
      })
    }
  }

  # dictionary parameters
  .dictionary_fun <- function(model_settings) {
    if (length(grep("dictionary_parameters", names(model_settings))) > 0) {
      x <- grep("dictionary_parameters", names(model_settings))
      lapply(seq_len(length(model_settings[[x]])), function(y) {
        if (length(grep(model_settings[[x]][[y]]$name, ls_param_group)) > 0) {
          if (is.null(model_settings[[x]][[y]]$used_for) || model_settings[[x]][[y]]$used_for == ui_step) {
            if (!is.null(model_settings[[x]][[y]]$tooltip)) {
              tooltip_tex <- model_settings[[x]][[y]]$tooltip
              # limit number of characters in tooltip box
              if (nchar(tooltip_tex) > 100) {
                tooltip_tex <- paste0(substr(tooltip_tex, start = 1, stop = 100), "...")
              }
            } else {
              tooltip_tex <- model_settings[[x]][[y]]$name
            }
            lapply(seq_len(length(model_settings[[x]][[y]]$default)), function(z) {
              fluidRow(use_bs_popover(),
                       column(4,
                              textInput(
                                inputId = ns(paste0("dictionary_parameters", y, z)),
                                label = paste(model_settings[[x]][[y]]$desc, names(model_settings[[x]][[y]]$default[z]),
                                              sep = ": "),
                                value = model_settings[[x]][[y]]$default[[z]]
                              )),
                       column(1,
                              actionButton(ns(paste0("tooltip_dict_", y)), "", icon = icon("info"),
                                           style='padding:4px; font-size:80%') %>%
                                bs_embed_popover(
                                  title = NULL,
                                  content = tooltip_tex
                                )
                       )
              )
            })
          }
        }
      })
    }
  }

  # boolean parameters
  .boolean_params_fun <- function(model_settings) {
    if (length(grep("boolean_parameters", names(model_settings))) > 0) {
      x <- grep("boolean_parameters", names(model_settings))
      lapply(seq_len(length(model_settings[[x]])), function(y) {
        if (length(grep(model_settings[[x]][[y]]$name, ls_param_group)) > 0) {
          if (!is.null(model_settings[[x]][[y]]$tooltip)) {
            tooltip_tex <- model_settings[[x]][[y]]$tooltip
            # limit number of characters in tooltip box
            if (nchar(tooltip_tex) > 100) {
              tooltip_tex <- paste0(substr(tooltip_tex, start = 1, stop = 100), "...")
            }
          } else {
            tooltip_tex <- model_settings[[x]][[y]]$name
          }
          if (!is.null(model_settings[[x]][[y]]$desc)) {
            label_widget <- model_settings[[x]][[y]]$desc
          } else {
            label_widget <- gsub("_", " ", model_settings[[x]][[y]]$name)
          }
          if (is.null(model_settings[[x]][[y]]$used_for) || model_settings[[x]][[y]]$used_for == ui_step) {
            fluidRow(use_bs_popover(),
                     column(4,
                            checkboxInput(
                              inputId = ns(paste0("boolean_parameters", y)),
                              label = label_widget %>% capitalize_first_letter(),
                              value = model_settings[[x]]$default
                            )),
                     column(1,
                            actionButton(ns(paste0("tooltip_bool_", y)), "", icon = icon("info"),
                                         style='padding:4px; font-size:80%')  %>%
                              bs_embed_popover(
                                title = NULL,
                                content = tooltip_tex
                              )
                     )
            )
          }
        }
      })
    }
  }

  # float parameters
  .float_fun <- function(model_settings) {
    if (length(grep("float_parameters", names(model_settings))) > 0) {
      x <- grep("float_parameters", names(model_settings))
      lapply(seq_len(length(model_settings[[x]])), function(y) {
        if (length(grep(model_settings[[x]][[y]]$name, ls_param_group)) > 0) {
          if (!is.null(model_settings[[x]][[y]]$tooltip)) {
            tooltip_tex <- model_settings[[x]][[y]]$tooltip
            # limit number of characters in tooltip box
            if (nchar(tooltip_tex) > 100) {
              tooltip_tex <- paste0(substr(tooltip_tex, start = 1, stop = 100), "...")
            }
          } else {
            tooltip_tex <- model_settings[[x]][[y]]$name
          }
          if (!is.null(model_settings[[x]][[y]]$desc)) {
            label_widget <- model_settings[[x]][[y]]$desc
          } else {
            label_widget <- gsub("_", " ", model_settings[[x]][[y]]$name)
          }
          if (!is.null(model_settings[[x]][[y]]$stepsize)) {
            step_size <- model_settings[[x]][[y]]$stepsize
          } else {
            step_size <- model_settings[[x]][[y]]$default
          }
          if (is.null(model_settings[[x]][[y]]$used_for) || model_settings[[x]][[y]]$used_for == ui_step) {
            fluidRow(use_bs_popover(),
                     column(4,
                            sliderInput(
                              inputId = ns(paste0("float_parameters", y)),
                              label = paste0(label_widget, ":") %>% capitalize_first_letter(),
                              min = model_settings[[x]][[y]]$min,
                              max = model_settings[[x]][[y]]$max,
                              step = step_size,
                              value = model_settings[[x]][[y]]$default
                            )
                     ),
                     column(1,
                            actionButton(ns(paste0("tooltip_float_", y)), "", icon = icon("info"),
                                         style='padding:4px; font-size:80%') %>%
                              bs_embed_popover(
                                title = NULL,
                                content = tooltip_tex
                              )
                     )
            )
          }
        }
      })
    }
  }

  # drop-down parameters
  .dropdown_fun <- function(model_settings) {
    if (length(grep("dropdown_parameters", names(model_settings))) > 0) {
      x <- grep("dropdown_parameters", names(model_settings))
      lapply(seq_len(length(model_settings[[x]])), function(y) {
        if (length(grep(model_settings[[x]][[y]]$name, ls_param_group)) > 0) {
          if (!is.null(model_settings[[x]][[y]]$tooltip)) {
            tooltip_tex <- model_settings[[x]][[y]]$tooltip
            # limit number of characters in tooltip box
            if (nchar(tooltip_tex) > 100) {
              tooltip_tex <- paste0(substr(tooltip_tex, start = 1, stop = 100), "...")
            }
          } else {
            tooltip_tex <- model_settings[[x]][[y]]$name
          }
          if (!is.null(model_settings[[x]][[y]]$desc)) {
            label_widget <- model_settings[[x]][[y]]$desc
          } else {
            label_widget <- gsub("_", " ", model_settings[[x]][[y]]$name)
          }
          list_values <- lapply(seq_len(length(model_settings[[x]][[y]]$options)), function (z) {
            model_settings[[x]][[y]]$options[[z]]$id
          })
          if (is.null(model_settings[[x]][[y]]$used_for) || model_settings[[x]][[y]]$used_for == ui_step) {
            fluidRow(use_bs_popover(),
                     column(4,
                            selectInput(
                              inputId = ns(paste0("dropdown_parameters", y)),
                              label = paste0(label_widget, sep = ": "),
                              choices = list_values,
                              selected = model_settings[[x]][[y]]$default,
                              multiple = TRUE
                            )),
                     column(1,
                            actionButton(ns(paste0("tooltip_dropdown_", y)), "", icon = icon("info"),
                                         style='padding:4px; font-size:80%') %>%
                              bs_embed_popover(
                                title = NULL,
                                content = tooltip_tex
                              )
                     )
            )
          }
        }
      })
    }
  }

  tagList(.event_set_fun_g(model_settings),
          .event_occurrence_fun_g(model_settings),
          .string_fun(model_settings),
          .list_fun(model_settings),
          .dictionary_fun(model_settings),
          .boolean_params_fun(model_settings),
          .float_fun(model_settings),
          .dropdown_fun(model_settings))
}


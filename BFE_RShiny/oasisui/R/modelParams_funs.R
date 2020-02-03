
#' Basic Output Configuration functions
#'
#' @description Functions for the Basic Output Configuration/Rerun Model Parameters view.
#'
#' @param session Current session.
#' @param model_settings Model settings retireved from the API.
#'
#' @return List of functions.
#'
#' @export
basicConfig_funs <- function(session, model_settings) {
  ns <- session$ns
  # Event set
  .event_set_fun <- function(model_settings) {
    selectInput(
      inputId = ns("event_set"),
      label = "Event Set:",
      choices = lapply(seq(1, length(model_settings$event_set.options)), function (x) {
        model_settings$event_set.options[[x]]$id}),
      selected = lapply(seq(1, length(model_settings$event_set.options)), function (x) {
        model_settings$event_set.options[[x]]$id}),
      multiple = FALSE
    )
  }

  # Event Occurrence
  .event_occurrence_fun <- function(model_settings) {
    selectInput(
      inputId = ns("event_occurrence"),
      label = "Event Occurrence:",
      choices = lapply(seq(1, length(model_settings$event_occurrence_id.options)), function (x) {
        model_settings$event_occurrence_id.options[[x]]$id}),
      selected = lapply(seq(1, length(model_settings$event_occurrence_id.options)), function (x) {
        model_settings$event_occurrence_id.options[[x]]$id}),
      multiple = FALSE
    )
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
#' @param model_settings Model settings retireved from the API.
#'
#' @return List of functions.
#'
#' @export
advancedConfig_funs <- function(session, model_settings) {
  ns <- session$ns
  # string parameters
  .string_fun <- function(model_settings) {
    if (length(grep("string_parameters", names(model_settings))) > 0) {
      lapply(grep("string_parameters", names(model_settings)), function (x) {
        selectInput(
          inputId = ns(paste0("string_parameters", x)),
          label = paste0(gsub("_", " ", model_settings[[x]]$name), ":") %>% capitalize_first_letter(),
          choices = model_settings[[x]]$default,
          selected = model_settings[[x]]$default,
          multiple = TRUE
        )
      })
    }
  }

  # list parameters
  .list_fun <- function(model_settings) {
    if (length(grep("list_parameters", names(model_settings))) > 0) {
      lapply(grep("list_parameters", names(model_settings)), function (x) {
        list_params <- model_settings[grep("list_parameters", names(model_settings))]
        textInput(
          inputId = ns(paste0("list_parameters", x)),
          label = paste0(gsub("_", " ", model_settings[[x]]$name), ":") %>% capitalize_first_letter(),
          value = paste(unlist(model_settings[[x]]$default), collapse = ", ")
        )
      })
    }
  }

  # dictionary parameters
  .dictionary_fun <- function(model_settings) {
    if (length(grep("dictionary_parameters", names(model_settings))) > 0) {
      lapply(grep("dictionary_parameters", names(model_settings)), function (x) {
        lapply(seq(1, length(model_settings[[x]]$default)), function (y) {
          textInput(
            inputId = ns(paste0("dictionary_parameters", y)),
            label = names(model_settings[[x]]$default[y]),
            value = model_settings[[x]]$default[[y]]
          )
        })
      })
    }
  }

  # boolean parameters
  .boolean_params_fun <- function(model_settings) {
    if (length(grep("boolean_parameters", names(model_settings))) > 0) {
      lapply(grep("boolean_parameters", names(model_settings)), function (x) {
        checkboxInput(
          inputId = ns(paste0("boolean_parameters", x)),
          label = gsub("_", " ", model_settings[[x]]$name) %>% capitalize_first_letter(),
          value = model_settings[[x]]$default
        )
      })
    }
  }

  # float parameters
  .float_fun <- function(model_settings) {
    if (length(grep("float_parameters", names(model_settings))) > 0) {
      lapply(grep("float_parameters", names(model_settings)), function (x) {
        sliderInput(
          inputId = ns(paste0("float_parameters", x)),
          label = paste0(gsub("_", " ", model_settings[[x]]$name), ":") %>% capitalize_first_letter(),
          min = model_settings[[x]]$min,
          max = model_settings[[x]]$max,
          value = model_settings[[x]]$default
        )
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
    .float_fun(model_settings))

}

#' Dynamic UI
#'
#' @description Add input fields for custom and drill-down configuration.
#'
#' @param session Current session.
#' @param analysisID Current analysis ID.
#' @param tag Current tag.
#' @param n_field Number of fields to produce.
#' @param oed_field Oed fields.
#'
#' @return UI element.
#'
#' @export
dynamicUI <- function(session, analysisID, tag, n_field, oed_field) {
  ns <- session$ns
  # only called for Case 2 and 3 (drill-down or custom)

  # all empty fields
  if (tag == default_tags[3]) {
    fluidRow(
      column(
        5,
        selectInput(
          inputId = ns(paste0("sinsummarylevels", n_field)),
          label = "Summary Levels",
          choices = c("All Risks", oed_field),
          selected = NULL,
          multiple = TRUE
        )
      ),
      column(
        5,
        selectInput(
          inputId = ns(paste0("sinreports", n_field)),
          label = "Reports",
          choices = output_options$variables,
          selected = NULL,
          multiple = TRUE
        )
      )
    )
  } else if (tag == default_tags[2]) {
    fluidRow(
      column(
        5,
        selectInput(
          inputId = ns(paste0("sinsummarylevels", n_field)),
          label = "Summary Levels",
          choices = oed_field,
          selected = NULL,
          multiple = TRUE
        )
      )
    )
  }
}

#' Rerun UI
#'
#' @description Summary level and reports fields in re-run situation.
#'
#' @param session Current session.
#' @param analysisID Current analysis ID.
#' @param tag Current tag.
#' @param oed_field Oed fields.
#'
#' @return UI element.
#'
#' @export
rerunUI <- function(session, analysisID, tag, oed_field) {

  ns <- session$ns

  # Reactive Values ------------------------------------------------------------
  result <- reactiveValues(
    # consecutive number of rows of output parameters - dependent on abuttonadd
    n_add = 0
  )
  # inserted fields
  inserted <- reactiveValues(val = 0)

  # retrieve run information from API
  out_cnfg_tbl <- session$userData$data_hub$get_ana_settings_content(
    analysisID, oasisapi = session$userData$oasisapi)

  if (length(out_cnfg_tbl$gul_summaries) > 0 ||
      length(out_cnfg_tbl$il_summaries) > 0 ||
      length(out_cnfg_tbl$ri_summaries) > 0) {

    if (length(out_cnfg_tbl$gul_summaries) > 0) {
      prsp_sum <- out_cnfg_tbl$gul_summaries
    } else if (length(out_cnfg_tbl$il_summaries) > 0) {
      prsp_sum <- out_cnfg_tbl$il_summaries
    } else {
      prsp_sum <- out_cnfg_tbl$ri_summaries
    }

    # summary levels
    choices_sum <- lapply(seq(1:length(prsp_sum)), function(x) {
      if (length(prsp_sum[[x]]$oed_fields) == 0) {
        prsp_sum[[x]]$oed_fields <- "All Risks"
      }
      unlist(prsp_sum[[x]]$oed_fields)
    })

    # reports
    choices_rep_final <- lapply(seq(1:length(prsp_sum)), function(x) {
      not_include <- c("id", "return_period_file", "lec_output", "oed_fields", "leccalc", "ord_output")
      # not_include <- c("id", "return_period_file", "lec_output", "oed_fields", "leccalc")
      names_reports <- as.list(names(prsp_sum[[x]]))
      names_reports <- names_reports[-which(names_reports %in% not_include)]
      if (length(names(prsp_sum[[x]]$leccalc)) > 0) {
        choices_rep_final <- c(names_reports, as.list(names(prsp_sum[[x]]$leccalc)))
      } else {
        choices_rep_final <- as.list(names_reports)
      }
      choices_rep_final <- unlist(varsdf$labels[which(varsdf$fields %in% choices_rep_final)])
      choices_rep_final
    })

    choices_prsp <- NULL
    if (out_cnfg_tbl$gul_output) {
      choices_prsp <- c(choices_prsp, "GUL")
    }
    if (out_cnfg_tbl$il_output) {
      choices_prsp <- c(choices_prsp, "IL")
    }
    if (out_cnfg_tbl$ri_output) {
      choices_prsp <- c(choices_prsp, "RI")
    }
    # update checkboxes selection
    updateCheckboxGroupInput(session, "chkboxgrplosstypes", selected = choices_prsp)

  }

  # first set of fields corresponds to 0, so if we e.g. have 3 in total, then we have added 2
  result$n_add <- length(choices_sum) - 1
  inserted$val <- seq(0, isolate(result$n_add))

  # update main panel
  if (tag == default_tags[3]) {
    lapply(seq(1, length(choices_sum)), function(x) {
      tags$div(
        id = x - 1,
        fluidRow(
          column(
            5,
            selectInput(
              inputId = ns(paste0("sinsummarylevels", x - 1)),
              label = "Summary Levels",
              choices = c("All Risks", oed_field),
              selected = choices_sum[[x]],
              multiple = TRUE
            )
          ),
          column(
            5,
            selectInput(
              inputId = ns(paste0("sinreports", x - 1)),
              label = "Reports",
              choices = output_options$variables,
              selected = choices_rep_final[[x]],
              multiple = TRUE
            )
          ))
      )
    })
  } else if (tag == default_tags[2]) {
    choices_sum <- choices_sum[choices_sum != "All Risks"]
    if (length(choices_sum) > 0) {
      lapply(seq(1, length(choices_sum)), function(x) {
        tags$div(
          id = x - 1,
          fluidRow(
            column(
              5,
              selectInput(
                inputId = ns(paste0("sinsummarylevels", x - 1)),
                label = "Summary Levels",
                choices = oed_field,
                selected = choices_sum[[x]],
                multiple = TRUE
              )
            )
          )
        )
      })
    } else {
      fluidRow(
        column(
          5,
          selectInput(
            inputId = ns(paste0("sinsummarylevels", 0)),
            label = "Summary Levels",
            choices = oed_field,
            selected = NULL,
            multiple = TRUE
          )
        )
      )
    }
  }
}


#' Dynamic UI with buttons
#'
#' @description Add "+" and "x" buttons to dynamic UI.
#'
#' @param session Current session.
#' @param analysisID Current analysis ID.
#' @param ana_flag Analysis flag.
#' @param tag Current tag.
#' @param oed_field Oed fields.
#'
#' @return UI element.
#'
#' @export
dynamicUI_btns <- function(session, analysisID, ana_flag, tag, oed_field) {
  ns <- session$ns
  if (tag == default_tags[2] || tag == default_tags[3]) {
    tagList(fluidRow(
      column(1,
             br(),
             actionButton(ns("addBtn"), label = "", icon = icon("plus")) %>%
               bs_embed_tooltip(title = defineSingleAna_tooltips$addBtn, placement = "right")
      ),
      column(2,
             br(),
             disabled(
               actionButton(
                 ns("removeBtn"),
                 label = "",
                 icon = icon("times")
               ) %>%
                 bs_embed_tooltip(title = defineSingleAna_tooltips$removeBtn, placement = "right")
             )
      ),
      column(8,
             if (ana_flag == "C") {
               dynamicUI(session, analysisID, tag, 0, oed_field)
             } else if (ana_flag == "R") {
               rerunUI(session, analysisID, tag, oed_field)
             }
      )
    ),
    tags$div(id = 'placeholder'))
  }
}

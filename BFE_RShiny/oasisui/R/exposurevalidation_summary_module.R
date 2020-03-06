# Exposure Validation Module ---------------------------------------------------

# UI ---------------------------------------------------------------------------
#' exposurevalidationsummaryUI
#' @rdname exposurevalidationsummary
#'
#' @description UI/View for exposure validation of an analysis.
#'
#' @return List of tags.
#'
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#'
#' @export
exposurevalidationsummaryUI <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(div(oasisuiRefreshButton(ns("abuttonSumexposurerefresh")), style = "margin-right: 25px;")),
    fluidRow(
      column(12,
             DTOutput(ns("dt_total_validation"))
      )
    ),
    fluidRow(
      column(12,
             selectInput(ns("input_peril"), label = "Pick peril", choices = NULL, multiple = TRUE)
      )
    ),
    fluidRow(
      column(12,
             DTOutput(ns("dt_summary_validation"))
      )
    ),
    fluidRow(
      column(12,
             plotlyOutput(ns("outputplot_vis"))
      )
    )
  )
}


# Server -----------------------------------------------------------------------

#' exposurevalidationsummary
#'
#' @rdname exposurevalidationsummary
#'
#' @description Server logic for exposure validation of an analysis.
#'
#' @template params-module
#' @template params-active
#' @param analysisID Selected analysis id.
#' @param counter Reactive value to trigger inputs download.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom tidyr gather
#'
#' @export
exposurevalidationsummary <- function(input,
                                      output,
                                      session,
                                      analysisID = reactive(NULL),
                                      counter = reactive(NULL),
                                      active = reactive(TRUE)) {

  ns <- session$ns

  # Params and Reactive Values -------------------------------------------------
  result <- reactiveValues(
    # dataframe summary
    summary_validation_tbl = NULL,
    # summary for all perils
    summary_tbl = NULL,
    # list perils
    perils = NULL
  )

  type_to_plot <- c("number of locations", "tiv")

  # Modeled exposure and uploaded exposure ------------------------------------
  observeEvent({
    active()
    counter()
  }, {
    if (length(active()) > 0 && active() && counter() > 0) {
      result$summary_tbl <- session$userData$data_hub$get_ana_validation_summary_content(analysisID())
      result$perils <- unique(result$summary_tbl$peril)
      keys_errors <- session$userData$data_hub$get_ana_dataset_content(id = analysisID(),
                                                                       dataset_identifier = "keys-errors.csv",
                                                                       type = "input")

      peril_id <- unique(keys_errors$PerilID)
      if (is.null(result$perils)) {
        peril_choices <- "no perils available for summary"
      } else if (!is.null(result$perils) && length(peril_id) == 0) {
        peril_choices <- result$perils
      } else {
        peril_choices <- paste0(result$perils, " (", peril_id, ")")
      }
      updateSelectInput(session, inputId = "input_peril", choices = peril_choices, selected = peril_choices)
      # TODO: if above leaves input_peril the same, we still want to call .reloadSummary once
    }
  })

  # Perils ---------------------------------------------------------------------
  observeEvent(input$input_peril, {
    if (!is.na(input$input_peril) && input$input_peril != "") {
      .reloadSummary(input_peril = input$input_peril)
    }
  })

  # Summary table --------------------------------------------------------------
  output$dt_total_validation <- renderDT(
    if (!is.null(result$summary_validation_tbl) && nrow(result$summary_validation_tbl) > 0) {
      # filter for only entries related to the total peril
      sum_tot_filter <- result$summary_validation_tbl %>% filter(peril == "total")
      # insert commas at thousands
      sum_tot_filter$modelled <- add_commas(sum_tot_filter$modelled)
      sum_tot_filter$`not-modelled` <- add_commas(sum_tot_filter$`not-modelled`)
      sum_tot_filter$portfolio <- add_commas(sum_tot_filter$portfolio)

      #drop unnecessary columns
      drops <- c("all", "fail", "success", "nomatch")
      sum_tot_filter <- sum_tot_filter[, !(names(c(sum_tot_filter)) %in% drops)]

      datatable(
        sum_tot_filter %>% capitalize_names_df(),
        class = "oasisui-table display",
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = getTableOptions()
      )
    }
  )

  output$dt_summary_validation <- renderDT(
    if (!is.null(result$summary_validation_tbl) && nrow(result$summary_validation_tbl) > 0) {
      # filter for all entries not related to the total peril
      sum_val_filter <- result$summary_validation_tbl %>% filter(peril != "total")

      # insert commas at thousands
      sum_val_filter$all <- add_commas(sum_val_filter$all)
      sum_val_filter$fail <- add_commas(sum_val_filter$fail)
      sum_val_filter$nomatch <- add_commas(sum_val_filter$nomatch)
      sum_val_filter$success <- add_commas(sum_val_filter$success)

      #drop unnecessary columns
      drops <- c("modelled", "not-modelled", "portfolio")
      sum_val_filter <- sum_val_filter[, !(names(c(sum_val_filter)) %in% drops)]

      datatable(
        sum_val_filter %>% capitalize_names_df(),
        class = "oasisui-table display",
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = getTableOptions()
      )
    } else {
      nothingToShowTable("Exposure validation summary not found")
    }
  )

  # Visualize Summary ----------------------------------------------------------
  observeEvent(result$summary_tbl, {
    if (!is.null(result$summary_tbl) && length(result$summary_tbl) > 0) {
      if (ncol(result$summary_tbl) > 3) {
        df_vis <- .extract_df_plot(df = result$summary_tbl)
        output$outputplot_vis <- renderPlotly({ggplotly(.plot_stack_hist(df = df_vis))})
      }
    }
  })

  # Refresh button -------------------------------------------------------------
  observeEvent(input$abuttonSumexposurerefresh, {
    # Get modeled locations
    result$summary_tbl <- session$userData$data_hub$get_ana_validation_summary_content(analysisID())
    withModalSpinner(
      .reloadSummary(input$input_peril),
      "Refreshing...",
      size = "s", t = 0.5
    )
  })


  # Utils functions ------------------------------------------------------------
  .extract_df_plot <- function(df) {
    df <- df %>%
      filter(type %in% type_to_plot) %>%
      mutate(fail = 100*fail/all,
             success = 100*success/all,
             nomatch = 100*nomatch/all,
             all = NULL) %>%
      gather(key, value, factor_key = TRUE, -c(peril, type)) %>%
      mutate(peril = as.factor(peril),
             type = as.factor(type))
    df
  }

  # visualize exposure validation summary
  .plot_stack_hist <- function(df) {
    brks <- c(0, 25, 50, 75, 100)
    lbs <- c("0%", "25%", "50%", "75%", "100%")
    n_plots_row <- ifelse(length(unique(df$peril)) < 4, length(unique(df$peril)), 4)
    # leave only: Fail, Success and Nomatch statuses and remove peril "total
    key_unwanted <- c("portfolio", "not-modelled", "modelled")
    key_unwanted_list <- unlist(lapply(seq(1, length(key_unwanted)), function(x) {grep(key_unwanted[x], df$key)}))
    if (length(key_unwanted_list) > 0) {
      df <- df[-key_unwanted_list, ]
    }
    if (length(grep("total", df$peril) > 0)) {
      df <- df[-grep("total", df$peril), ]
    }
    status <- df$key
    percentage <- df$value

    p <- ggplot(data = df, aes(x = type, y = percentage, fill = status)) +
      theme(
        plot.title = element_blank(),
        text = element_text(size = 12),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey45", size = 0.5),
        axis.line.y = element_line(color = "grey45", size = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title =  element_blank(),
        legend.position = "right"
      ) +
      geom_bar(position = "stack", stat = "identity") +
      scale_fill_manual(values = c("fail" = "#f29c24", "success" = "#128e37", "nomatch" = "#1f77b4", "match" = "#FF7F0E")) +
      scale_y_continuous(breaks = brks, labels = lbs) +
      facet_wrap(df$peril, ncol = n_plots_row)
    # p
  }

  # reload summary table
  .reloadSummary <- function(input_peril) {
    logMessage(".reloadSummary called")
    # Clean up df
    result$summary_validation_tbl <- NULL
    # Build df
    if (!is.null(result$summary_tbl) && length(result$summary_tbl) > 0 && !is.null(input_peril)) {
      # match inputs to perils
      perils_match <- unlist(lapply(seq(1, length(result$perils)), function(x) {
        y <- grep(result$perils[x], input$input_peril)
        which(input$input_peril[y] == result$summary_tbl$peril)
      }))
      result$summary_validation_tbl <- result$summary_tbl[perils_match,]
    } else {
      result$summary_validation_tbl <- NULL
    }
    result$summary_validation_tbl
  }

  invisible()
}

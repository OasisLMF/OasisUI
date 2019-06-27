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
    fluidRow(div(oasisuiRefreshButton(ns("abuttonexposurerefresh")), style = "margin-right: 25px;")),
    fluidRow(
      column(12,
             selectInput(ns("input_peril"), label = "Pick peril", choices = NULL)
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
#' @importFrom dplyr case_when
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT DTOutput
#' @importFrom jsonlite read_json
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
#' @importFrom stats setNames
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom tidyr unite
#' @importFrom tidyr separate
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
    #dataframe summary
    summary_validation_tbl = NULL,
    #summary for all perils
    summary_tbl = NULL,
    #list perils
    perils = NULL
  )

  type_to_plot <- c("number of locations", "tiv")

  # Modeled exposure and uploaded exposure ------------------------------------
  observeEvent({
    active()
    counter()
  }, {
    if (length(active()) > 0 && active() && counter() > 0) {
      result$summary_tbl <-  session$userData$data_hub$get_ana_validation_summary_content(analysisID())
      result$perils <- unique(result$summary_tbl$peril)
      updateSelectInput(session, inputId = "input_peril", choices = ifelse(!is.null(result$perils), result$perils, "no perils available for summary"))
    }
  })

  # Perils ---------------------------------------------------------------------
  observeEvent(input$input_peril, {
    if (!is.na(input$input_peril) && input$input_peril != "") {
      .reloadSummary(input_peril = input$input_peril)
    }
  })

  # Summary table --------------------------------------------------------------

  output$dt_summary_validation <- renderDT(
    if (!is.null(result$summary_validation_tbl) && nrow(result$summary_validation_tbl) > 0) {
      datatable(
        result$summary_validation_tbl %>% capitalize_names_df(),
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

  # Vusualize Summary ----------------------------------------------------------

  observeEvent(result$summary_tbl, {
    if (!is.null(result$summary_tbl) && length(result$summary_tbl) > 0) {
      df_vis <- .extract_df_plot(df = result$summary_tbl)
      output$outputplot_vis <- renderPlotly({ggplotly(.plot_stack_hist(df = df_vis) )})
    }
  })


  # Refresh button -------------------------------------------------------------
  onclick("abuttonexposurerefresh", {
    # Get modeled locations
    withModalSpinner(
      result$summary_tbl <-  session$userData$data_hub$get_ana_validation_summary_content(analysisID()),
      "Refreshing...",
      size = "s"
    )
    .reloadSummary()
  })

  # Utils functions ------------------------------------------------------------

  .extract_df_plot <- function(df){
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

  # # visualize exposure validation summary
  .plot_stack_hist <- function(df) {
    brks <- c(0, 25, 50, 75, 100)
    lbs <- c("0%", "25%", "50%", "75%", "100%")
    n_plots_row <- ifelse(length(unique(df$peril)) < 4, length(unique(df$peril)), 4)
    p <- ggplot(data = df, aes(x = df$type, y = df$value, fill = df$key)) +
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
    p
  }

  # reload dataframes

  #reload summary table
  .reloadSummary <- function(input_peril){

    logMessage(".reloadSummary called")

    # Clean up df
    result$summary_validation_tbl <- NULL

    #Build df
    if (!is.null(result$summary_tbl) && length(result$summary_tbl) > 0 && !is.null(input_peril)) {
      result$summary_validation_tbl <- result$summary_tbl %>%
        filter(peril == input_peril) %>%
        select(-peril)
    } else {
      result$summary_validation_tbl <- NULL
    }
  }

}

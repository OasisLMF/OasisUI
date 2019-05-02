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
    fluidRow(div(flamingoRefreshButton(ns("abuttonexposurerefresh")), style = "margin-right: 25px;")),
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
#' @param portfolioID Selected portfolio ID.
#' @param counter Reactive value to trigger inputs download.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT DTOutput
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
#' @importFrom tidyr spread
#' @importFrom tidyr replace_na
#' @importFrom tidyr gather
#'
#' @export
exposurevalidationsummary <- function(input,
                                      output,
                                      session,
                                      analysisID = "",
                                      portfolioID = "",
                                      counter = reactive(NULL),
                                      active = reactive(TRUE)) {

  ns <- session$ns

  # Params and Reactive Values -------------------------------------------------
  result <- reactiveValues(
    # checked locations
    uploaded_locs_check = NULL,
    #dataframe summary
    summary_validation_tbl = NULL,
    #summary for all perils
    summary_lst = NULL
  )

  loc_ids <- "location_ids"

  # Modeled exposure and uploaded exposure ------------------------------------
  observeEvent({
    active()
    counter()
  }, {
    if (length(active()) > 0 && active() && counter() > 0) {
      .reloadExposureValidation()
      result$summary_lst <- .read_summary(analysisID())
      updateSelectInput(session, inputId = "input_peril", choices = names(result$summary_lst)[names(result$summary_lst) != "all"])
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
        class = "flamingo-table display",
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

  observeEvent(result$summary_validation_tbl, {
    if (!is.null(result$summary_validation_tbl) && nrow(result$summary_validation_tbl)) {

      df_vis <- lapply(result$summary_validation_tbl$type, function(g){
        g_label <- g %>%
          gsub(pattern = "_", replacement = " ") %>%
          gsub(pattern = "\\.", replacement = " ") %>%
          capitalize_first_letter()
        df_to_plot <- .extract_df_plot(df = result$summary_validation_tbl, filter_row = g, all = result$summary_lst[["all"]]) %>%
          mutate(gridcol = g_label)
        df_to_plot
      }) %>%
        bind_rows( .id = "column_label")

      df_vis <- df_vis %>%
        mutate(gridcol = as.factor(gridcol))
      output$outputplot_vis <- renderPlotly({ggplotly(.plot_stack_hist(df = df_vis) )})
    }
  })


  # Refresh button -------------------------------------------------------------
  onclick("abuttonexposurerefresh", {
    # Get modeled locations
    withModalSpinner(
      api_get_analyses_input_file(analysisID()),
      "Refreshing...",
      size = "s"
    )
    .reloadExposureValidation()
    .reloadSummary()
  })

  # Utils functions ------------------------------------------------------------

  .extract_df_plot <- function(df,filter_row, all){
    all_df <- t(all) %>%
      as.data.frame() %>%
      mutate(type = rownames(.),
             all = as.numeric(V1),
             V1 = NULL)

    df <- left_join(df, all_df, by = "type")
    tot <- df$all[df$type == filter_row]
    df_sel <- df %>%
      filter(type == filter_row) %>%
      select(-type) %>%
      gather(key, value)
    has_commas <- lapply(seq(nrow(df_sel)),
                         function(x){grepl(",", df_sel$value[x])}) %>%
      unlist() %>%
      any()
    if (!has_commas) {
      df_sel$value <- as.numeric(df_sel$value)
    }
    df_sel <- df_sel %>%
      mutate(value = sapply(seq(nrow(df_sel)),
                            function(x) {
                              if (df_sel$key[x] != "all" && !is.numeric(df_sel$value[x])) {
                                df_sel$value[x] %>%
                                  strsplit(",") %>%
                                  unlist() %>% length()
                              } else {
                                df_sel$value[x]
                              }
                            })) %>%
      mutate(value = as.numeric(value)/tot)

    df_sel1 <- df_sel %>%
      filter(key %in% c("fail", "success")) %>%
      mutate(ref = 1)

    df_sel2 <- df_sel %>%
      rbind(c("match", 1 - df_sel$value[df_sel$key == "nomatch"])) %>%
      filter(key %in% c("nomatch", "match")) %>%
      mutate(ref = 2)

    df <- rbind(df_sel1, df_sel2) %>%
      mutate(value = as.numeric(value)*100,
             key = as.factor(key))

    df
  }

  # # visualize exposure validation summary
  .plot_stack_hist <- function(df) {
    brks <- c(0, 25, 50, 75, 100)
    lbs <- c("0%", "25%", "50%", "75%", "100%")
    p <- ggplot(data = df, aes(x = df$ref, y = df$value, fill = df$key)) +
      theme(
        plot.title = element_blank(),
        text = element_text(size = 12),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "grey45", size = 0.5),
        axis.line.y = element_line(color = "grey45", size = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title =  element_blank(),
        legend.position = "right"
      ) +
      geom_bar(position = "stack", stat = "identity") +
      scale_fill_manual(values = c("fail" = "#db1e2a", "success" = "#128e37", "nomatch" = "#1f77b4", "match" = "#FF7F0E")) +
      scale_y_continuous(breaks = brks, labels = lbs) +
      facet_wrap(df$gridcol, ncol = 3)
    p
  }

  # dummy read summary.json
  .read_summary <- function(anaID){

    logMessage(".read_summary called")

    forig <- "./www/summary.json"
    json_lst <- jsonlite::read_json(forig, simplifyVector = TRUE)

    #drop portfolio and modle data
    json_lst <- json_lst[sapply(json_lst, function(x) {is.list(x)})]

    json_lst_all <- json_lst[["all"]] %>%
      as.data.frame()

    #drop field all data
    json_lst["all"] <- NULL

    reg_expr_dot <- "^([^.]+)[.](.+)$"

    lst_peril <- lapply(json_lst, function(json_lst_curr) {
      df <- lapply(
        json_lst_curr[sapply(json_lst_curr, function(x) {!is.atomic(x)})],
        function(x) {
          x[[loc_ids]] <- toString(x[[loc_ids]], width = 20)
          x}) %>%
        as.data.frame()

      df <- df %>%
        format(scientific = FALSE) %>%
        t() %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        mutate(key = sub(reg_expr_dot, "\\1", rownames(.)),
               type = sub(reg_expr_dot, "\\2", rownames(.))) %>%
        spread(key, 1, convert = TRUE)

      df
    })

    lst_peril$all <- cbind(data.frame(location_ids = nrow(result$uploaded_locs_check)),json_lst_all)

    lst_peril

  }

  # reload dataframes

  #reload summary table
  .reloadSummary <- function(input_peril){

    logMessage(".reloadSummary called")

    # Clean up df
    result$summary_validation_tbl <- NULL

    #Build df
    if (!is.null(result$summary_lst) && length(result$summary_lst) > 0 && !is.null(input_peril)) {
      result$summary_validation_tbl <- result$summary_lst[[input_peril]]
    } else {
      result$summary_validation_tbl <- NULL
    }
  }

  #dummy for exposure location comparison
  .reloadExposureValidation <- function(){

    logMessage(".reloadExposureValidation called")

    uploaded_locs_check <- check_loc(analysisID(), portfolioID())

    #updating reactive only when needed
    if (!identical(uploaded_locs_check,result$uploaded_locs_check)) {
      result$uploaded_locs_check <- uploaded_locs_check
    }

  }

}

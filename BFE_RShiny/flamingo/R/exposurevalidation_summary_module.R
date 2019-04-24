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
             DTOutput(ns("dt_summary_validation"))
      )
    ),
    fluidRow(
      column(12,
             h4("Locations Overview")
      ),
      column(6,
             plotlyOutput(ns("outputplot_loc1"))),
      column(6,
             plotlyOutput(ns("outputplot_loc2"))
      )
    ),
    fluidRow(
      column(12,
             h4("TIV Overview")
      ),
      column(6,
             plotlyOutput(ns("outputplot_tiv1"))),
      column(6,
             plotlyOutput(ns("outputplot_tiv2"))
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
#' @param analysisID Selected analysis id.
#' @param portfolioID selected portfolio ID.
#' @param counter Reactive value storing actionButton status.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom DT DTOutput
#' @importFrom plotly plot_ly
#' @importFrom plotly renderPlotly
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
                                      counter = NULL,
                                      active = reactive(TRUE)) {

  ns <- session$ns

  # Params and Reactive Values -------------------------------------------------
  result <- reactiveValues(
    #dataframe summary
    summary_validation_tbl = NULL
  )

  #name of element with location ids
  loc_ids <- "location_ids"

  color_success <- c('rgb(31,119,180)', 'rgb(255,127,14)')
  color_match <- c('rgb(114,147,203)', 'rgb(211,94,96)')

  # Modeled exposure and uploaded exposure ------------------------------------
  observeEvent({
    active()
    counter()
  }, {
    if (length(active()) > 0 && active() && counter() > 0) {
      .reloadExposureValidation()
      .reloadSummary()
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

      df_loc <- .extract_df_pieplot(df = result$summary_validation_tbl, filter_row = loc_ids)
      output$outputplot_loc1 <- renderPlotly({.plotly_pie(df = df_loc$df_sel1, pie_labs =  df_loc$df_sel1$key, pie_vals =  df_loc$df_sel1$value, colors2use = color_success)})
      output$outputplot_loc2 <- renderPlotly({.plotly_pie(df = df_loc$df_sel2, pie_labs = df_loc$df_sel2$key, pie_vals = df_loc$df_sel2$value, colors2use = color_match)})


      df_tiv <- .extract_df_pieplot(df = result$summary_validation_tbl, filter_row = "tiv")
      output$outputplot_tiv1 <- renderPlotly({.plotly_pie(df = df_tiv$df_sel1, pie_labs =  df_tiv$df_sel1$key, pie_vals =  df_tiv$df_sel1$value, colors2use = color_success)})
      output$outputplot_tiv2 <- renderPlotly({.plotly_pie(df = df_tiv$df_sel2, pie_labs = df_tiv$df_sel2$key, pie_vals = df_tiv$df_sel2$value, colors2use = color_match)})

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

  .extract_df_pieplot <- function(df,filter_row){
    tot <- df$all[df$type == filter_row] %>% as.numeric()
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
      filter(key %in% c("fail", "success"))

    df_sel2 <- df_sel %>%
      rbind(c("match", 1 - df_sel$value[df_sel$key == "nomatch"])) %>%
      filter(key %in% c("nomatch", "match"))

    return(list(
      df_sel1 = df_sel1,
      df_sel2 = df_sel2
    ))
  }

  # visualize exposure validation summary
  .plotly_pie <- function(df, pie_labs, pie_vals, pie_title, colors2use){
    pie_ly <- plot_ly(df, labels = pie_labs, values = as.numeric(pie_vals), type = 'pie', marker = list(colors = colors2use))
    pie_ly
  }

  # dummy read summary.json
  .read_summary <- function(anaID){

    logMessage(".read_summary called")

    forig <- "./www/summary.json"
    json_lst <- jsonlite::read_json(forig)

    reg_expr_dot <- "^([^.]+)[.](.+)$"

    df <- lapply(
      json_lst[sapply(json_lst, function(x) {!is.atomic(x)})],
      function(x) {x[[loc_ids]] <- toString(x[[loc_ids]], width = 20); x}) %>%
      as.data.frame()

    df <- df %>%
      format(scientific = FALSE) %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      mutate(key = sub(reg_expr_dot, "\\1", rownames(.)),
             type = sub(reg_expr_dot, "\\2", rownames(.))) %>%
      spread(key, 1, convert = TRUE)
    df
  }

  # reload dataframes

  #reload summary table
  .reloadSummary <- function(){

    logMessage(".reloadSummary called")

    # Clean up df
    result$summary_validation_tbl <- NULL

    #Build df
    if (!is.null(result$uploaded_locs_check) && nrow(result$uploaded_locs_check) > 0) {
      df <- .read_summary(analysisID())
      replace_lst <- lapply(names(df),
                            function(x){toString(nrow(result$uploaded_locs_check))}
      ) %>%
        setNames(names(df))
      result$summary_validation_tbl <- df %>%
        replace_na(replace_lst)
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

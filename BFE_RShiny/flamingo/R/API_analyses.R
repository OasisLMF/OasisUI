# R functions calling Analyses OasisAPI Calls ----------------------------------

#' Return analyses data for DT
#'
#' @rdname return_tbl_analysesData
#'
#' @description Returns a dataframe of analyses ready for being rendered as a data table.
#'
#' @param name Name of the analyses.
#' @param oasisapi as stored in session$userData$oasisapi
#'
#' @return Dataframe of previously posted analyses. Default empty string returns all analyses.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr contains
#' @importFrom dplyr arrange
#' @importFrom dplyr sym
#' @importFrom dplyr desc
#' @importFrom dplyr case_when
#'
#' @export
return_tbl_analysesData <- function(name = "", oasisapi) {

  .addIcons <- function(df) {
    StatusGood <- "RUN_COMPLETED"
    StatusBad <- c("INPUTS_GENERATION_ERROR", "RUN_ERROR", NA_character_)
    StatusAvailable <- "READY"

    # replace status in df
    if (!is.null(df)) {
      logMessage(paste0("replacing icons"))
      df <- df %>%
        mutate(status = case_when(status %in% StatusGood ~ Status$Completed,
                                  status %in% StatusBad ~ Status$Failed,
                                  status %in% StatusAvailable ~ Status$Ready,
                                  status %notin% c(StatusBad, StatusGood, StatusAvailable) ~ Status$Processing)) %>%
        as.data.frame()
    }
    df
  }

  tbl_analysesData <- oasisapi$return_df("analyses", list(name = name))
  if (!is.null(tbl_analysesData) && nrow(tbl_analysesData) > 0 && is.null(tbl_analysesData$detail)) {
    tbl_analysesData <- tbl_analysesData %>%
      select(-contains("file")) %>%
      as.data.frame()

    tbl_analysesData <- convert_created_modified(tbl_analysesData)
    tbl_analysesData <- tbl_analysesData %>%
      mutate(status_detailed = tolower(gsub(pattern = "_", " ", tbl_analysesData[, tbl_analysesDataNames$status]))) %>%
      arrange(desc(!! sym(tbl_analysesDataNames$id))) %>%
      .addIcons() %>%
      select(c(!! sym(tbl_analysesDataNames$id), !! sym(tbl_analysesDataNames$name),
               !! sym(tbl_analysesDataNames$portfolio), !! sym(tbl_analysesDataNames$model),
               !! sym(tbl_analysesDataNames$modified), !! sym(tbl_analysesDataNames$created),
               !! sym(tbl_analysesDataNames$status_detailed), !! sym(tbl_analysesDataNames$status)))

  } else {
    tbl_analysesData <- NULL
  }

  tbl_analysesData
}


#' Return analysis table in nice format for DT
#'
#' @rdname return_tbl_analysesData_nice
#'
#' @description Returns a dataframe of analyses prettified for being rendered as a data table.
#'
#' @param tbl_analysesData dataframe of analyses
#'
#' @return Dataframe of analyses
#'
#' @importFrom tidyr unite
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr sym
#'
#' @export
return_tbl_analysesData_nice <- function(tbl_analysesData) {
  # fetch model data to merge in table
    tbl_modelsData <- return_tbl_modelsData() %>%
      mutate(supplier = !! sym(tbl_modelsDataNames$supplier_id)) %>%
      select(!! sym(tbl_modelsDataNames$id), !! sym(tbl_modelsDataNames$model_id), supplier, !! sym(tbl_modelsDataNames$version_id))
  # fetch portfolio data to merge in table
    tbl_portfoliosData <- return_tbl_portfoliosData() %>%
      select(!! sym(tbl_portfoliosDataNames$id), !! sym(tbl_portfoliosDataNames$name)) %>%
      rename("portfolio_name" = tbl_portfoliosDataNames$name)

    admin_mode <- getOption("flamingo.settings.admin.mode")

  tbl_analysesData <- tbl_analysesData %>%
    left_join(tbl_modelsData, by = c("model" = "id")) %>%
    unite("model_version", c(tbl_modelsDataNames$model_id, tbl_modelsDataNames$version_id), sep = ", version ") %>%
    left_join(tbl_portfoliosData, by = c("portfolio" = "id"))

 if (admin_mode == "admin") {
   tbl_analysesData <- tbl_analysesData %>%
     select(!! sym(tbl_analysesDataNames$id),
            !! sym(tbl_analysesDataNames$name),
            !! sym(tbl_analysesDataNames$portfolio),
            portfolio_name,
            !! sym(tbl_analysesDataNames$model),
            model_version,
            supplier,
            !! sym(tbl_analysesDataNames$created),
            !! sym(tbl_analysesDataNames$modified),
            !! sym(tbl_analysesDataNames$status_detailed),
            !! sym(tbl_analysesDataNames$status)) %>%
     rename("portfolio_id" = tbl_analysesDataNames$portfolio) %>%
     rename("model_id" = tbl_analysesDataNames$model)
 } else {
   tbl_analysesData <- tbl_analysesData %>%
     select(!! sym(tbl_analysesDataNames$id),
            !! sym(tbl_analysesDataNames$name),
            portfolio_name,
            model_version,
            supplier,
            !! sym(tbl_analysesDataNames$created),
            !! sym(tbl_analysesDataNames$modified),
            !! sym(tbl_analysesDataNames$status_detailed),
            !! sym(tbl_analysesDataNames$status))
 }
  tbl_analysesData <- tbl_analysesData %>%
    capitalize_names_df()
  tbl_analysesData
}

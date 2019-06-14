# Analyses API Calls -----------------------------------------------------------

#' Get analyses id
#'
#' Returns the specific analysis entry.
#'
#' @rdname api_get_analyses_id
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Previously posted analyses id.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_analyses_id <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "", sep = "/")
  ))

  response <- api_fetch_response("GET", request_list)

  api_handle_response(response)
}

#' Delete analyses id
#'
#' Removes an analysis.
#'
#' @rdname api_delete_analyses_id
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Response to analysis deletion.
#'
#' @importFrom httr DELETE
#' @importFrom httr add_headers
#'
#' @export
api_delete_analyses_id <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "", sep = "/")
  ))

  response <- api_fetch_response("DELETE", request_list)

  api_handle_response(response)
}

#' Post analyses
#'
#' Creates an analysis based on the input data.
#'
#' @rdname api_post_analyses
#'
#' @param name The name of the analysis.
#' @param portfolio The id of the portfolio.
#' @param model The id of the model.
#'
#' @return The posted analysis.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#'
#' @export
api_post_analyses <- function(name, portfolio, model) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept =  get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(name = name, portfolio = portfolio, model = model),
    encode = "json",
    path = paste(get_version(), "analyses", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

# R functions calling Analyses API Calls ---------------------------------------

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

#' Return analysis details for DT
#'
#' @rdname return_tbl_analysisdetails
#'
#' @description Returns a dataframe of analysis details ready for being rendered as a data table.
#'
#' @param id A unique integer value identifying this analysis.
#'
#' @return Dataframe of details of previously posted analysis.
#'
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom dplyr case_when
#' @importFrom tidyr gather
#'
#' @export
return_tbl_analysisdetails <- function(id) {

  # helper function to replace variable with icon
  .replacewithIcon <- function(var) {
    # status code for files
    status_code_exist <- 200
    status_code_notfound <- 404

    var <- case_when(var %in% status_code_exist ~ Status$Completed,
                     var %in% status_code_notfound ~ Status$Processing,
                     var %notin% c(status_code_notfound, status_code_exist) ~ Status$Failed)
    return(var)
  }

  tbl_analysisdetails <- return_df(api_get_analyses_id, id)

  if (!is.null(tbl_analysisdetails) && nrow(tbl_analysisdetails) > 0 && is.null(tbl_analysisdetails$detail)) {
    tbl_analysisdetails <-  tbl_analysisdetails %>%
      select(contains("file") ) %>%
      as.data.frame()
    # replace files with Icons
    # input file
    get_analyses_input_file <- api_get_analyses_input_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$input_file]] <- toString(get_analyses_input_file$result$status_code) %>%
      .replacewithIcon()
    # settings file
    get_analyses_settings_file <- api_get_analyses_settings_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$settings_file]] <- toString(get_analyses_settings_file$result$status_code) %>%
      .replacewithIcon()
    # input errors file
    get_analyses_input_errors_file <- api_get_analyses_input_errors_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$input_errors_file]] <- toString(get_analyses_input_errors_file$result$status_code) %>%
      .replacewithIcon()
    # input generation traceback file
    get_analyses_input_generation_traceback_file <- api_get_analyses_input_generation_traceback_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$input_generation_traceback_file]] <- toString(get_analyses_input_generation_traceback_file$result$status_code) %>%
      .replacewithIcon()
    # output file
    get_analyses_input_file <- api_get_analyses_input_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$output_file]] <- toString(get_analyses_input_file$result$status_code) %>%
      .replacewithIcon()
    # run traceback file
    get_analyses_run_traceback_file <- api_get_analyses_run_traceback_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$run_traceback_file]] <- toString(get_analyses_run_traceback_file$result$status_code) %>%
      .replacewithIcon()
    # reshape df
    tbl_analysisdetails <- gather(tbl_analysisdetails,  key = "files", value = "status") %>%
      as.data.frame()
  } else {
    tbl_analysisdetails <- NULL
  }

  tbl_analysisdetails
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

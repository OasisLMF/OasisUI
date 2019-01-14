# Analyses API Calls -----------------------------------------------------------

#' Get analyses
#'
#' Returns a list of analysis objects.
#'
#' @rdname api_get_analyses
#'
#' @param name the name of the analysis. Default empty string returns all analyses
#'
#' @return previously posted analyses.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_analyses <- function(name = "") {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", "", sep = "/"),
    query = list(name = name)
  )
  
  api_handle_response(response)
}

#' Get analyses id
#'
#' Returns the specific analysis entry.
#'
#' @rdname api_get_analyses_id
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return previously posted analyses id.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_analyses_id <- function(id) {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "", sep = "/")
  )
  
  api_handle_response(response)
}

#' Delete analyses id
#'
#' Removes an analysis.
#'
#' @rdname api_delete_analyses_id
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return presponse to analysis deletion.
#'
#' @importFrom httr DELETE
#' @importFrom httr add_headers
#'
#' @export
api_delete_analyses_id <- function(id) {
  
  response <- DELETE(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "", sep = "/")
  )
  
  api_handle_response(response)
}

#' Post analyses
#'
#' Creates an analysis based on the input data
#'
#' @rdname api_post_analyses
#'
#' @param name the name of the analysis.
#' @param portfolio the id of the portfolio.
#' @param model the id of the model.
#'
#' @return the posted analysis.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#'
#' @export
api_post_analyses <- function(name, portfolio, model) {
  
  response <- POST(
    get_url(),
    config = add_headers(
      Accept =  get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(name = name, portfolio = portfolio, model = model),
    encode = "json",
    path = paste(get_version(), "analyses", "", sep = "/")
  )
  
  api_handle_response(response)
}


# R functions calling Analyses API Calls ---------------------------------------

#' Return analyses Data fot DT
#'
#' @rdname return_tbl_analysisData
#'
#' @description Returns a dataframe of analyses ready for being rendered as a data table
#'
#' @param name name of the analyses.
#'
#' @return dataframe of previously posted analyses. Default empty string returns all analyses.
#'
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom dplyr arrange
#' @importFrom dplyr sym
#' @importFrom dplyr desc
#' @importFrom dplyr case_when
#'
#' @export
return_tbl_analysesData <- function(name = ""){
  
  .replaceWithIcons <- function(df){
    #Status
    StatusGood <- c("RUN_COMPLETED")
    StatusBad <- c("INPUTS_GENERATION_ERROR", "RUN_ERROR", NA_character_)
    StatusAvailable <- c("READY")
    
    #Replace Status in df
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
  
  tbl_analysesData <- return_df(api_get_analyses, name)
  
  if (!is.null(tbl_analysesData) && nrow(tbl_analysesData) > 0 && is.null(tbl_analysesData$detail)) {
    tbl_analysesData <- tbl_analysesData %>%
      select(-contains("file") ) %>%
      as.data.frame()
    
    tbl_analysesData <- convert_created_modified(tbl_analysesData) 
    tbl_analysesData <- tbl_analysesData %>%
      arrange(desc(!! sym(tbl_analysesDataNames$id))) %>%
      .replaceWithIcons() %>%
      select(c(!! sym(tbl_analysesDataNames$id), !! sym(tbl_analysesDataNames$name),
               !! sym(tbl_analysesDataNames$portfolio), !! sym(tbl_analysesDataNames$model),
               !! sym(tbl_analysesDataNames$modified), !! sym (tbl_analysesDataNames$created),
               !! sym(tbl_analysesDataNames$status)))
    
  } else {
    tbl_analysesData <- NULL
  }
  
  return(tbl_analysesData)
}


#' Return analysis Details fot DT
#'
#' @rdname return_tbl_analysisdetails
#'
#' @description Returns a dataframe of analysis details ready for being rendered as a data table
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return dataframe of details of previously posted analysis
#'
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom dplyr arrange
#' @importFrom dplyr sym
#' @importFrom dplyr desc
#' @importFrom dplyr case_when
#'
#' @export
return_tbl_analysisdetails <- function(id){
  
  #Help function to replace variable with icon
  .replacewithIcon <- function(var){
    # Staus Code for files
    status_code_exist <- 200
    status_code_notfound <- 404
    
    var <- case_when(var %in% status_code_exist ~ Status$Completed,
                     var %in% status_code_notfound ~ Status$Processing,
                     var %notin% c(status_code_notfound, status_code_exist) ~ Status$Failed)
    return(var)
  }

  tbl_analysisdetails <- return_df(api_get_analyses_id,id) 
  
  if (!is.null(tbl_analysisdetails) && nrow(tbl_analysisdetails) > 0 && is.null(tbl_analysisdetails$detail)) {
    tbl_analysisdetails <-  tbl_analysisdetails %>%
      select(contains("file") ) %>%
      as.data.frame()
    #Replace files with Icons
    #Input File
    get_analyses_input_file <- api_get_analyses_input_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$input_file]] <- toString(get_analyses_input_file$result$status_code) %>%
      .replacewithIcon()
    #Setting File
    get_analyses_settings_file <- api_get_analyses_settings_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$settings_file]] <- toString(get_analyses_settings_file$result$status_code) %>%
      .replacewithIcon()
    #Input errors file
    get_analyses_input_errors_file <- api_get_analyses_input_errors_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$input_errors_file]] <- toString(get_analyses_input_errors_file$result$status_code) %>%
      .replacewithIcon()
    #input generation traceback file
    get_analyses_input_generation_traceback_file <- api_get_analyses_input_generation_traceback_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$input_generation_traceback_file]] <- toString(get_analyses_input_generation_traceback_file$result$status_code) %>%
      .replacewithIcon()
    #output file
    get_analyses_input_file <- api_get_analyses_input_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$output_file]] <- toString(get_analyses_input_file$result$status_code) %>%
      .replacewithIcon()
    #run traceback file
    get_analyses_run_traceback_file <- api_get_analyses_run_traceback_file(id)
    tbl_analysisdetails[[tbl_analysesDataNames$run_traceback_file]] <- toString(get_analyses_run_traceback_file$result$status_code) %>%
      .replacewithIcon()
    # reshape df
    tbl_analysisdetails <- gather(tbl_analysisdetails,  key = "files", value = "status") %>%
      as.data.frame()
    
  } else {
    tbl_analysisdetails <- NULL
  }
  
  return(tbl_analysisdetails)
}

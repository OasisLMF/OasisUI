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
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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
  
  logWarning = warning
  
  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))
  
  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
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
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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
  
  logWarning = warning
  
  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))
  
  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
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
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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
  
  logWarning = warning
  
  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))
  
  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
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
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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
  
  logWarning = warning
  
  # re-route potential warning for logging
  tryCatch(warn_for_status(response),
           warning = function(w) logWarning(w$message))
  
  structure(
    list(
      status = http_status(response)$category,
      result = response
    ),
    class = c("apiresponse")
  )
}


# R functions calling Analyses API Calls ---------------------------------------

#' Return analyses Dataframe
#' 
#' @rdname return_analyses_df
#' 
#' @description Returns a dataframe of analyses
#' 
#' @param name name of the analysis.
#' 
#' @return dataframe of previously posted analyses Default empty string returns all analyses.
#' 
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' 
#' @export
return_analyses_df <- function(name = ""){
  get_analyses <- api_get_analyses(name)
  analysesList <- content(get_analyses$result)
  analyses_df <- bind_rows(analysesList) %>% 
    as.data.frame()
  return(analyses_df)
}

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
        mutate(tatus = tolower(status)) %>%
        mutate(status = case_when(status %in% StatusGood ~ StatusCompleted,
                                  status %in% StatusBad ~ StatusFailed,
                                  status %in% StatusAvailable ~ StatusReady,
                                  status %notin% c(StatusBad, StatusGood, StatusAvailable) ~ StatusProcessing)) %>%
        as.data.frame()
    }
    df
  }
  
  tbl_analysesData <- return_analyses_df(name) %>%
    select(-contains("file") ) %>% 
    as.data.frame()
  idx <- tbl_analysesData[[tbl_analysesData.AnaID]]
  numpf <- length(idx)
  for (i in seq(numpf) ) {
    tbl_analysesData[i, tbl_analysesData.AnaCreated] <- toString(as.POSIXct(tbl_analysesData[i, tbl_analysesData.AnaCreated] , format = "%d-%m-%YT%H:%M:%S"))
    tbl_analysesData[i, tbl_analysesData.AnaModified] <- toString(as.POSIXct(tbl_analysesData[i, tbl_analysesData.AnaModified], format = "%d-%m-%YT%H:%M:%S"))
  }
  tbl_analysesData <- tbl_analysesData %>%
    arrange(desc(!! sym(tbl_analysesData.AnaID))) %>%
    .replaceWithIcons() %>%
    select(c(!! sym(tbl_analysesData.AnaID), !! sym(tbl_analysesData.AnaName),
             !! sym(tbl_analysesData.PortfolioID), !! sym(tbl_analysesData.ModelID),
             !! sym(tbl_analysesData.AnaModified), !! sym (tbl_analysesData.AnaCreated),
             !! sym(tbl_analysesData.AnaStatus)))
  return(tbl_analysesData)
}


#' Return analysis details Dataframe
#' 
#' @rdname return_analyses_id_df
#' 
#' @description Returns a dataframe of analysis details
#' 
#' @param id a unique integer value identifying this analysis.
#' 
#' @return dataframe of details of previously posted analysis
#' 
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' 
#' @export
return_analyses_id_df <- function(id){
  get_analyses_id <- api_get_analyses_id(id)
  analysesIdList <- content(get_analyses_id$result)
  analyses_id_df <- bind_rows(analysesIdList) %>% 
    as.data.frame()
  return(analyses_id_df)
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
    var <- case_when(var %in% status_code_exist ~ StatusCompleted,
                     var %in% status_code_notfound ~ StatusProcessing,
                     var %notin% c(status_code_notfound, status_code_exist) ~ StatusFailed)
    return(var)
  }
  
  tbl_analysisdetails <- return_analyses_id_df(id) %>%
    select(contains("file") ) %>% 
    as.data.frame()
  #Replace files with Icons
    #Input File
    get_analyses_input_file <- api_get_analyses_input_file(id) 
    tbl_analysisdetails[[tbl_analysesData.AnaInputFile]] <- toString(get_analyses_input_file$result$status_code) %>%
      .replacewithIcon()
    #Setting File
    get_analyses_settings_file <- api_get_analyses_settings_file(id) 
    tbl_analysisdetails[[tbl_analysesData.AnaSettingFile]] <- toString(get_analyses_settings_file$result$status_code) %>%
      .replacewithIcon()
    #Input errors file
    get_analyses_input_errors_file <- api_get_analyses_input_errors_file(id) 
    tbl_analysisdetails[[tbl_analysesData.AnaInputErrFile]] <- toString(get_analyses_input_errors_file$result$status_code) %>%
      .replacewithIcon()
    #input generation traceback file
    get_analyses_input_generation_traceback_file <- api_get_analyses_input_generation_traceback_file(id) 
    tbl_analysisdetails[[tbl_analysesData.AnaInputGenTraceBackFile]] <- toString(get_analyses_input_generation_traceback_file$result$status_code) %>%
      .replacewithIcon()
    #output file
    get_analyses_input_file <- api_get_analyses_input_file(id) 
    tbl_analysisdetails[[tbl_analysesData.AnaOutputFile]] <- toString(get_analyses_input_file$result$status_code) %>%
      .replacewithIcon()
    #run traceback file
    get_analyses_run_traceback_file <- api_get_analyses_run_traceback_file(id) 
    tbl_analysisdetails[[tbl_analysesData.AnaRunTracebackFile]] <- toString(get_analyses_run_traceback_file$result$status_code) %>%
      .replacewithIcon()
  # reshape df
    tbl_analysisdetails <- gather(tbl_analysisdetails,  key = "files", value = "status") %>%
    as.data.frame()
  return(tbl_analysisdetails)
}

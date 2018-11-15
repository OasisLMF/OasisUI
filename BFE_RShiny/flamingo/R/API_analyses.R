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
    StatusBad <- c("INPUTS_GENERATION_ERROR", NA_character_)
    StatusAvailable <- c("READY")
    
    #Replace status in df
    if (!is.null(df)) {
      logMessage(paste0("replacing icons"))
      df <- df %>%
        mutate(status = tolower(status)) %>%
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

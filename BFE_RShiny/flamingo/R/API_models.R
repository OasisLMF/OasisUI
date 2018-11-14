# Models API Calls -------------------------------------------------------------

#' Get models
#' 
#' Returns a list of model objects.
#' 
#' @rdname api_get_models
#' 
#' @param supplier_id the supplier ID for the model. Default is empty string.
#' 
#' @return previously posted models. . Default empty string returns all portfolios.
#' 
#' @importFrom httr GET
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_get_models <- function(supplier_id = "") {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "models", "", sep = "/"),
    query = list(`supplier_id` = supplier_id)
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

# R functions calling Models API Calls -----------------------------------------

#' Return Models Dataframe
#' 
#' @rdname return_models_df
#' 
#' @description Returns a dataframe of models
#' 
#' @param supplier_id the supplier ID for the model. Default is empty string.
#' 
#' @return dataframe of previously posted models. Default empty string returns all portfolios.
#' 
#' @importFrom dplyr bind_rows
#' 
#' @export
return_models_df <- function(supplier_id = ""){
  get_models <- api_get_models(supplier_id)
  modelsList <- content(get_models$result)
  models_df <- bind_rows(modelsList) %>% 
    as.data.frame()
  return(models_df)
}

#' Return Models Data fot DT
#' 
#' @rdname return_tbl_modelsData
#' 
#' @description Returns a dataframe of models ready for being rendered as a data table
#' 
#' @param supplier_id the supplier ID for the model. Default is empty string.
#' 
#' @return dataframe of previously posted modelss. Default empty string returns all models.
#' 
#' @importFrom dplyr arrange
#' @importFrom dplyr sym
#' @importFrom dplyr desc
#' 
#' @export
return_tbl_modelsData <- function(supplier_id = ""){

  
  tbl_modelsData <- return_models_df(supplier_id) %>%
    as.data.frame()
  idx <- tbl_modelsData[[tbl_modelsData.ModelId]]
  numpf <- length(idx)
  for (i in seq(numpf) ) {
    tbl_modelsData[i, tbl_modelsData.ModelCreated] <- toString(as.POSIXct(tbl_modelsData[i, tbl_modelsData.ModelCreated] , format = "%d-%m-%YT%H:%M:%S"))
    tbl_modelsData[i, tbl_modelsData.ModelModified] <- toString(as.POSIXct(tbl_modelsData[i, tbl_modelsData.ModelModified], format = "%d-%m-%YT%H:%M:%S"))
  }
  tbl_modelsData <- tbl_modelsData %>%
    arrange(desc(!! sym(tbl_modelsData.ModelId)))
  return(tbl_modelsData)
}

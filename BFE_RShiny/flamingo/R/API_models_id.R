# Model Resources file ---------------------------------------------------------

#' Get model resource file
#'
#' Gets the model resource_file contents
#'
#' @rdname api_get_models_id_resource_file
#'
#' @param id a unique integer value identifying this model.
#'
#' @return previously posted model resource file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr warn_for_status
#' @importFrom httr http_status
#'
#' @export
api_get_models_id_resource_file <- function(id) {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "models", id, "resource_file", "", sep = "/")
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

#' Return model resource file Dataframe
#' 
#' @rdname return_models_resource_file_content
#' 
#' @description Returns a dataframe of model resource file
#' 
#' @param id a unique integer value identifying this model.
#' 
#' @return dataframe of resource file of previously posted model
#' 
#' @importFrom httr content
#' 
#' @export
return_models_id_resource_file_content <- function(id){
  get_models_id_resource_file <- api_get_models_id_resource_file(id)
  modelsIdResourceFileList <- content(get_models_id_resource_file$result)
  return(modelsIdResourceFileList)
}


#' Return model resource file Dataframe
#' 
#' @rdname return_models_resource_file_df
#' 
#' @description Returns a dataframe of model resource file
#' 
#' @param id a unique integer value identifying this model.
#' 
#' @return dataframe of resource file of previously posted model
#' 
#' @importFrom dplyr bind_rows
#' 
#' @export
return_models_id_resource_file_df <- function(id){
  modelsIdResourceFileList <- return_models_id_resource_file_content(id)
  models_id_resource_file_df <- unlist(modelsIdResourceFileList) %>%
    bind_rows() %>%
    as.data.frame(stringsAsFactors = FALSE)
  resource_file_df <- data.frame(
    resource = names(models_id_resource_file_df), 
    content = t(models_id_resource_file_df)[,1], 
    stringsAsFactors = FALSE)
  return(resource_file_df)
}
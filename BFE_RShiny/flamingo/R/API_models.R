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

  api_handle_response(response)
}

#' Get models id
#'
#' Returns the specific model entry.
#'
#' @rdname api_get_models_id
#'
#' @param id a unique integer value identifying this analysis.
#'
#' @return previously posted models id.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_models_id <- function(id) {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "models", id, "", sep = "/")
  )

  api_handle_response(response)
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
#' @return dataframe of previously posted models. Default empty string returns all models.
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
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


#' Return Model Dataframe
#'
#' @rdname return_model_df
#'
#' @description Returns a dataframe of model
#'
#' @param id the ID for the model.
#'
#' @return dataframe of previously posted model.
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#'
#' @export
return_model_df <- function(id){
  get_model <- api_get_models_id(id)
  modelList <- content(get_model$result)
  model_df <- bind_rows(modelList) %>%
    as.data.frame()
  return(model_df)
}

#' Return Model Data fot DT
#'
#' @rdname return_tbl_modelData
#'
#' @description Returns a dataframe of model ready for being rendered as a data table
#'
#' @param id the ID for the model.
#'
#' @return dataframe of previously posted model.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr sym
#' @importFrom dplyr desc
#'
#' @export
return_tbl_modelData <- function(id){
  tbl_modelData <- return_model_df(id) %>%
    as.data.frame()
    tbl_modelData[[tbl_modelsData.ModelCreated]] <- toString(as.POSIXct(tbl_modelData[[tbl_modelsData.ModelCreated]], format = "%d-%m-%YT%H:%M:%S"))
    tbl_modelData[[tbl_modelsData.ModelModified]] <- toString(as.POSIXct(tbl_modelData[[tbl_modelsData.ModelModified]], format = "%d-%m-%YT%H:%M:%S"))
  return(tbl_modelData)
}

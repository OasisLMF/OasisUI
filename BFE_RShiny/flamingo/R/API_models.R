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
  tbl_modelsData <- return_df(api_get_models, supplier_id)
  
  if (!is.null(tbl_modelsData) && nrow(tbl_modelsData) > 0 && is.null(tbl_modelsData$detail)) {
    tbl_modelsData <- convert_created_modified(tbl_modelsData)
    tbl_modelsData <- tbl_modelsData %>%
      arrange(desc(!! sym(tbl_modelsDataNames$id)))
  } else {
    tbl_modelsData <- NULL
  }

  return(tbl_modelsData)
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
  tbl_modelData <- return_df(api_get_models_id, id)
  
  if (!is.null(tbl_modelData) && nrow(tbl_modelData) > 0  && is.null(tbl_modelData$detail) ) {
    tbl_modelData <- convert_created_modified(tbl_modelData)
  } else {
    tbl_modelData <- NULL
  }
  
  return(tbl_modelData)
}

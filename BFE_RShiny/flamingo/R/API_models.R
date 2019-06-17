# R functions calling Models API Calls -----------------------------------------
#' Return models data for DT
#'
#' @rdname return_tbl_modelsData
#'
#' @description Returns a dataframe of models ready for being rendered as a data table.
#'
#' @param supplier_id The supplier ID for the model. Default is empty string.
#' @param oasisapi as stored in session$userData$oasisapi
#'
#' @return Dataframe of previously posted models. Default empty string returns all models.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr sym
#' @importFrom dplyr desc
#'
#' @export
return_tbl_modelsData <- function(oasisapi, supplier_id = "") {
  tbl_modelsData <-  oasisapi$return_df("models", api_param = list(`supplier_id` = supplier_id))
  if (!is.null(tbl_modelsData) && nrow(tbl_modelsData) > 0 && is.null(tbl_modelsData$detail)) {
    tbl_modelsData <- convert_created_modified(tbl_modelsData)
    tbl_modelsData <- tbl_modelsData %>%
      arrange(desc(!! sym(tbl_modelsDataNames$id)))
  } else {
    tbl_modelsData <- NULL
  }

  tbl_modelsData
}

#' Return model data for DT
#'
#' @rdname return_tbl_modelData
#'
#' @description Returns a dataframe of model ready for being rendered as a data table.
#'
#' @param id The ID for the model.
#'
#' @return Dataframe of previously posted model.
#'
#' @export
return_tbl_modelData <- function(id) {
  tbl_modelsData <-  oasisapi$return_df(paste("models", id,  sep = "/"))

  if (!is.null(tbl_modelData) && nrow(tbl_modelData) > 0  && is.null(tbl_modelData$detail)) {
    tbl_modelData <- convert_created_modified(tbl_modelData)
  } else {
    tbl_modelData <- NULL
  }

  tbl_modelData
}

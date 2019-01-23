# Model Resources file ---------------------------------------------------------
#' Get model resource file
#'
#' Gets the model resource_file contents.
#'
#' @rdname api_get_models_id_resource_file
#'
#' @param id A unique integer value identifying this model.
#'
#' @return Previously posted model resource file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_models_id_resource_file <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "models", id, "resource_file", "", sep = "/")
  ))

  response <- api_fetch_response("GET", request_list)

  api_handle_response(response)
}

#' Return model resource file dataframe
#'
#' @rdname return_models_resource_file_df
#'
#' @description Returns a dataframe of a model resource file.
#'
#' @param id A unique integer value identifying this model.
#'
#' @return Dataframe of resource file of previously posted model.
#'
#' @importFrom dplyr bind_rows
#'
#' @export
return_models_id_resource_file_df <- function(id) {
  modelsIdResourceFileList <- return_response(api_get_models_id_resource_file, id)
  models_id_resource_file_df <- unlist(modelsIdResourceFileList) %>%
    bind_rows() %>%
    as.data.frame(stringsAsFactors = FALSE)

  data.frame(
    resource = names(models_id_resource_file_df),
    content = t(models_id_resource_file_df)[, 1],
    stringsAsFactors = FALSE
  )
}

# Model Resources file ---------------------------------------------------------

#' Get model resource file
#'
#' Gets the model resource_file contents
#'
#' @rdname api_get_models_resource_file
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
api_get_models_resource_file <- function(id) {
  
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
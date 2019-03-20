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
#' @importFrom dplyr intersect
#' @importFrom purrr flatten
#' @importFrom tidyr unite
#'
#' @export
return_models_id_resource_file_df <- function(id) {
  modelsIdResourceFileList <- return_response(api_get_models_id_resource_file, id)
  modelsList_names <- names(modelsIdResourceFileList)
  return_list <- lapply(modelsList_names, function(i){ # i <- modelsList_names[1]
    curr_list <- flatten(flatten(modelsIdResourceFileList[i]))
    curr_list_names <- names(curr_list)
    lst_names <- lapply(curr_list_names, function(k){names(curr_list[[k]])}) #k <- curr_list_names[1]
    colsResources <- Reduce(intersect, lst_names)
    colsmust <- setdiff(c("values", "name"), colsResources)
    if (length(colsmust) > 0) {
      df <- data.frame(matrix(ncol = length(colsResources) + 1, nrow = 0), stringsAsFactors = FALSE)
      names(df) <- c(colsResources, colsmust)
    } else {
      df <- data.frame(matrix(ncol = length(colsResources), nrow = 0), stringsAsFactors = FALSE)
      names(df) <- colsResources
    }
    for (j in curr_list_names) {# j <- curr_list_names[1]
      curr_df <- unlist(curr_list[[j]]) %>%
        bind_rows() %>%
        as.data.frame(stringsAsFactors = FALSE)
      cols2merge <- setdiff(names(curr_df), colsResources)
      colmissing <- setdiff(unique(c(colsmust,colsResources)), names(curr_df))
      if (length(cols2merge) > 0) {
        curr_df <- curr_df %>%
          unite(col = "values", c(cols2merge), sep = ", " )
      } else if (length(colmissing) > 0 ) {
        df2add <- c("-")
        names(df2add) <- colmissing
        curr_df <- c(curr_df, df2add) %>% as.data.frame()
      }
      if ("name" %notin% names(curr_df)) {
        curr_df <- cbind(curr_df, data.frame(name = j))
      }
      df <- rbind(df, curr_df)
    }
   df
  })
  names(return_list) <- modelsList_names
  return_list
}

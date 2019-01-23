#' Return element based on length of input
#'
#' @rdname showname
#'
#' @description Returns the element name of the input x if x has length > 1, the
#'   element x itself if it has length = 1, and "Not available" if x has length
#'   = 0.
#'
#' @param x List, NULL or string.
#'
#' @return String.
#'
#' @export
showname <- function(x) {
  if (length(x) > 1) {
    y <- x$name
  } else if (length(x) == 0) {
    y <- "Not Available"
  } else {
    y <- x
  }
  y
}

#' Return response from API query
#'
#' @rdname return_response
#'
#' @description Returns a list of the API response.
#'
#' @param api_query Function representing the API query.
#' @param api_param Parameter for the api_query.
#'
#' @return List of the API response.
#'
#' @importFrom httr content
#'
#' @export
return_response <- function(api_query, api_param = "") {
  get_response <- api_query(api_param)
  responseList <- content(get_response$result)
  responseList
}

#' Return dataframe from API response
#'
#' @rdname return_df
#'
#' @description Returns a dataframe of the API response.
#'
#' @param api_query Function representing the API query.
#' @param api_param Parameter for the api_query.
#'
#' @return Dataframe of the API response.
#'
#' @importFrom dplyr bind_rows
#'
#' @export
return_df <- function(api_query, api_param = "") {

  responseList <- return_response(api_query, api_param)

  if (length(responseList) > 0) {
    if (length(responseList[[1]]) > 1 ) {
      responseList <- lapply(responseList, function(x) {lapply(x, showname)})
    } else {
      responseList <- lapply(responseList, showname)
    }
    response_df <- bind_rows(responseList) %>%
      as.data.frame()
  } else {
    response_df <- NULL
  }

  response_df
}

#' Return file as Dataframe
#'
#' @rdname return_file_df
#'
#' @description Returns a dataframe of a file downloaded from the API.
#'
#' @param api_query Function representing the API query.
#' @param api_param Parameter for the api_query.
#'
#' @return Dataframe of a file downloaded from the API.
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#'
#' @export
return_file_df <- function(api_query, api_param = "") {
  fileList <- content(api_query(api_param)$result)
  if (is.null(names(fileList))) {
    file_df <- strsplit(fileList, split = "\n") %>%
      as.data.frame(stringsAsFactors = FALSE)
    colnames(file_df) <- file_df[1, ]
  } else {
    file_df <- bind_rows(fileList) %>%
      as.data.frame()
  }
  file_df
}

#' Convert created and modified columns
#'
#' @rdname convert_created_modified
#'
#' @description Replaces created and modified columns with different date format.
#'
#' @param tbl_obj Dataframe to convert.
#'
#' @return Dataframe with converted columns.
#'
#' @export
convert_created_modified <- function(tbl_obj) {
  tbl_obj_names <- names(tbl_obj)
  numpf <- nrow(tbl_obj)
  for (i in seq(numpf)) {
    tbl_obj[i, "created"] <- toString(as.POSIXct(tbl_obj[i, "created"], format = "%d-%m-%YT%H:%M:%S"))
    tbl_obj[i, "modified"] <- toString(as.POSIXct(tbl_obj[i, "modified"], format = "%d-%m-%YT%H:%M:%S"))
  }
  tbl_obj
}

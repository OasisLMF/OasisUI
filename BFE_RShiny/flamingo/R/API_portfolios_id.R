# Location File ----------------------------------------------------------------
#' Get portfolios location file
#'
#' Gets the portfolios location_file contents
#'
#' @rdname api_get_portfolios_location_file
#'
#' @param id a unique integer value identifying this portfolio.
#'
#' @return previously posted portfolios location files.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_portfolios_location_file <- function(id) {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", id, "location_file", "", sep = "/")
  )

  api_handle_response(response)
}

#' Post portfolios location file
#'
#' Sets the portfolios location_file contents.
#'
#' @rdname api_post_portfolios_location_file
#'
#' @param id a unique integer value identifying this analysis.
#' @param filepath_location path to the location file.
#'
#' @return the posted portfolio location file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_portfolios_location_file <- function(id, filepath_location) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_location)),
    encode = "multipart",
    path = paste(get_version(), "portfolios", id, "location_file", "", sep = "/")
  )

  api_handle_response(response)
}

#' Return location_file Dataframe
#'
#' @rdname return_location_file_df
#'
#' @description Returns a dataframe of location_file
#'
#' @param id A unique integer value identifying the portfolio.
#'
#' @return Dataframe of location_file.
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#'
#' @export
return_location_file_df <- function(id) {
  location_fileList <- content(api_get_portfolios_location_file(id)$result)
  if (is.null(names(location_fileList))) {
    location_file_df <- strsplit(location_fileList, split = "\n") %>%
      as.data.frame(stringsAsFactors = FALSE)
    colnames(location_file_df) <- location_file_df[1, ]
  } else {
    location_file_df <- bind_rows(location_fileList) %>%
      as.data.frame()
  }
  return(location_file_df)
}

# Account File -----------------------------------------------------------------

#' Get portfolios accounts file
#'
#' Gets the portfolios accounts_file contents
#'
#' @rdname api_get_portfolios_accounts_file
#'
#' @param id a unique integer value identifying this portfolio.
#'
#' @return the previously posted portfolio accounts file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_portfolios_accounts_file <- function(id) {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", id, "accounts_file", "", sep = "/")
  )

  api_handle_response(response)
}

#' Post portfolios accounts file
#'
#' Sets the portfolios accounts_file contents.
#'
#' @rdname api_post_portfolios_accounts_file
#'
#' @param id a unique integer value identifying this analysis.
#' @param filepath_accounts path to accounts file.
#'
#' @return the posted portfolio accounts file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_portfolios_accounts_file <- function(id, filepath_accounts) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_accounts)),
    encode = "multipart",
    path = paste(get_version(), "portfolios", id, "accounts_file", "", sep = "/")
  )

  api_handle_response(response)
}

#' Return accounts_file Dataframe
#'
#' @rdname return_accounts_file_df
#'
#' @description Returns a dataframe of accounts_file.
#'
#' @param id A unique integer value identifying the portfolio.
#'
#' @return Dataframe of accounts_file.
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#'
#' @export
return_accounts_file_df <- function(id) {
  accounts_fileList <- content(api_get_portfolios_accounts_file(id)$result)
  if (is.null(names(accounts_fileList))) {
    accounts_file_df <- strsplit(accounts_fileList, split = "\n") %>%
      as.data.frame(stringsAsFactors = FALSE)
    colnames(accounts_file_df) <- accounts_file_df[1, ]
  } else {
    accounts_file_df <- bind_rows(accounts_fileList) %>%
      as.data.frame()
  }
  return(accounts_file_df)
}

# Reinsurance Info File -------------------------------------------------------

#' Get portfolios reinsurance info file
#'
#' Gets the portfolios reinsurance_info_file contents
#'
#' @rdname api_get_portfolios_reinsurance_info_file
#'
#' @param id a unique integer value identifying this portfolio.
#'
#' @return the previously posted portfolio reinsurance info file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_portfolios_reinsurance_info_file <- function(id) {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", id, "reinsurance_info_file", "", sep = "/")
  )

  api_handle_response(response)
}

#' Post portfolios reinsurance info file
#'
#' Sets the portfolios reinsurance_info contents.
#'
#' @rdname api_post_portfolios_reinsurance_info
#'
#' @param id A unique integer value identifying this analysis.
#' @param filepath_reinsurance_info Path to reinsurance info file.
#'
#' @return The posted portfolio reinsurance info file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_portfolios_reinsurance_info_file <- function(id, filepath_reinsurance_info) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_reinsurance_info)),
    encode = "multipart",
    path = paste(get_version(), "portfolios", id, "reinsurance_info_file", "", sep = "/")
  )

  api_handle_response(response)
}

#' Return reinsurance_info_file dataframe
#'
#' @rdname return_reinsurance_info_file_df
#'
#' @description Returns a dataframe of reinsurance_info_file.
#'
#' @param id A unique integer value identifying the portfolio.
#'
#' @return Dataframe of reinsurance_info_file.
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#'
#' @export
return_reinsurance_info_file_df <- function(id) {
  reinsurance_info_fileList <- content(api_get_portfolios_reinsurance_info_file(id)$result)
  if (is.null(names(reinsurance_info_fileList))) {
    reinsurance_info_file_df <- strsplit(reinsurance_info_fileList, split = "\n") %>%
      as.data.frame(stringsAsFactors = FALSE)
    colnames(reinsurance_info_file_df) <- reinsurance_info_file_df[1, ]
  } else {
    reinsurance_info_file_df <- bind_rows(reinsurance_info_fileList) %>%
      as.data.frame()
  }
  return(reinsurance_info_file_df)
}

# Reinsurance Source File ------------------------------------------------------

#' Get portfolios reinsurance source file
#'
#' Gets the portfolios reinsurance_source_file contents
#'
#' @rdname api_get_portfolios_reinsurance_source_file
#'
#' @param id A unique integer value identifying this portfolio.
#'
#' @return The previously posted portfolio reinsurance source file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_portfolios_reinsurance_source_file <- function(id) {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", id, "reinsurance_source_file", "", sep = "/")
  )

  api_handle_response(response)
}

#' Post portfolios reinsurance source file
#'
#' Sets the portfolios reinsurance_source contents.
#'
#' @rdname api_post_portfolios_reinsurance_source
#'
#' @param id A unique integer value identifying this analysis.
#' @param filepath_reinsurance_source Path to reinsurance source file.
#'
#' @return the posted portfolio reinsurance source file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_portfolios_reinsurance_source_file <- function(id, filepath_reinsurance_source) {

  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_reinsurance_source)),
    encode = "multipart",
    path = paste(get_version(), "portfolios", id, "reinsurance_source_file", "", sep = "/")
  )

  api_handle_response(response)
}

#' Return reinsurance_source_file dataframe
#'
#' @rdname return_reinsurance_source_file_df
#'
#' @description Returns a dataframe of reinsurance_source_file.
#'
#' @param id A unique integer value identifying the portfolio.
#'
#' @return Dataframe of reinsurance_source_file.
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#'
#' @export
return_reinsurance_source_file_df <- function(id) {
  reinsurance_source_fileList <- content(api_get_portfolios_reinsurance_source_file(id)$result)
  if (is.null(names(reinsurance_source_fileList))) {
    reinsurance_source_file_df <- strsplit(reinsurance_source_fileList, split = "\n") %>%
      as.data.frame(stringsAsFactors = FALSE)
    colnames(reinsurance_source_file_df) <- reinsurance_source_file_df[1, ]
  } else {
    reinsurance_source_file_df <- bind_rows(reinsurance_source_fileList) %>%
      as.data.frame()
  }
  return(reinsurance_source_file_df)
}

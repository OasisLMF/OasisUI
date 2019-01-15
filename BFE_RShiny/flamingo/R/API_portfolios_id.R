# Location File ----------------------------------------------------------------
#' Get portfolios location file
#'
#' Gets the portfolios location_file contents.
#'
#' @rdname api_get_portfolios_location_file
#'
#' @param id A unique integer value identifying this portfolio.
#'
#' @return Previously posted portfolios location files.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_portfolios_location_file <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", id, "location_file", "", sep = "/")
  ))

  response <- api_fetch_response("GET", request_list)

  api_handle_response(response)
}

#' Post portfolios location file
#'
#' Sets the portfolios location_file contents.
#'
#' @rdname api_post_portfolios_location_file
#'
#' @param id A unique integer value identifying this analysis.
#' @param filepath_location Path to the location file.
#'
#' @return The posted portfolio location file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_portfolios_location_file <- function(id, filepath_location) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_location)),
    encode = "multipart",
    path = paste(get_version(), "portfolios", id, "location_file", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

# Account File -----------------------------------------------------------------
#' Get portfolios accounts file
#'
#' Gets the portfolios accounts_file contents.
#'
#' @rdname api_get_portfolios_accounts_file
#'
#' @param id A unique integer value identifying this portfolio.
#'
#' @return The previously posted portfolio accounts file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_portfolios_accounts_file <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", id, "accounts_file", "", sep = "/")
  ))

  response <- api_fetch_response("GET", request_list)

  api_handle_response(response)
}

#' Post portfolios accounts file
#'
#' Sets the portfolios accounts_file contents.
#'
#' @rdname api_post_portfolios_accounts_file
#'
#' @param id A unique integer value identifying this analysis.
#' @param filepath_accounts Path to accounts file.
#'
#' @return The posted portfolio accounts file.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr upload_file
#'
#' @export
api_post_portfolios_accounts_file <- function(id, filepath_accounts) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_accounts)),
    encode = "multipart",
    path = paste(get_version(), "portfolios", id, "accounts_file", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

# Reinsurance Info File -------------------------------------------------------
#' Get portfolios reinsurance info file
#'
#' Gets the portfolios reinsurance_info_file contents.
#'
#' @rdname api_get_portfolios_reinsurance_info_file
#'
#' @param id A unique integer value identifying this portfolio.
#'
#' @return The previously posted portfolio reinsurance info file.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_portfolios_reinsurance_info_file <- function(id) {

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", id, "reinsurance_info_file", "", sep = "/")
  ))

  response <- api_fetch_response("GET", request_list)

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

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_reinsurance_info)),
    encode = "multipart",
    path = paste(get_version(), "portfolios", id, "reinsurance_info_file", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

# Reinsurance Source File ------------------------------------------------------
#' Get portfolios reinsurance source file
#'
#' Gets the portfolios reinsurance_source_file contents.
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

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", id, "reinsurance_source_file", "", sep = "/")
  ))

  response <- api_fetch_response("GET", request_list)

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

  request_list <- expression(list(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_reinsurance_source)),
    encode = "multipart",
    path = paste(get_version(), "portfolios", id, "reinsurance_source_file", "", sep = "/")
  ))

  response <- api_fetch_response("POST", request_list)

  api_handle_response(response)
}

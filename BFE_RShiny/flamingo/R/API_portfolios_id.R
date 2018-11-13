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
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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

# Reinsurance Infor File -------------------------------------------------------

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
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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

#' Post portfolios reinsurance info file
#' 
#' Sets the portfolios reinsurance_info contents.
#' 
#' @rdname api_post_portfolios_reinsurance_info
#' 
#' @param id a unique integer value identifying this analysis.
#' @param filepath_reinsurance_infos path to reinsurance info file.
#' 
#' @return the posted portfolio reinsurance info file. 
#' 
#' @importFrom httr POST
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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

# Reinsurance Source File ------------------------------------------------------

#' Get portfolios reinsurance source file
#' 
#' Gets the portfolios reinsurance_source_file contents
#' 
#' @rdname api_get_portfolios_reinsurance_source_file
#' 
#' @param id a unique integer value identifying this portfolio.
#' 
#' @return the previously posted portfolio reinsurance source file. 
#' 
#' @importFrom httr GET
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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

#' Post portfolios reinsurance source file
#' 
#' Sets the portfolios reinsurance_source contents.
#' 
#' @rdname api_post_portfolios_reinsurance_source
#' 
#' @param id a unique integer value identifying this analysis.
#' @param filepath_reinsurance_source path to reinsurance source file.
#' 
#' @return the posted portfolio reinsurance source file. 
#' 
#' @importFrom httr POST
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
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

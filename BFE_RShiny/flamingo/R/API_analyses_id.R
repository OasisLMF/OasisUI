# Input File -------------------------------------------------------------------
#' Get analysis input file
#' 
#' Gets the analysis input_file contents
#' 
#' @rdname api_get_analyses_input_file
#' 
#' @param id a unique integer value identifying this analysis.
#' 
#' @return previously posted analysis input file. 
#' 
#' @importFrom httr GET 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_get_analyses_input_file <- function(id) {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "input_file", "", sep = "/")
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

#' Post analysis input file
#' 
#' Sets the analysis input_file contents.
#' 
#' @rdname api_post_analyses_input_file
#' 
#' @param id a unique integer value identifying this analysis.
#' @param filepath_input path to the input file.
#' 
#' @return the posted analysis input file. 
#' 
#' @importFrom httr POST 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' @importFrom httr upload_file
#' 
#' @export
api_post_analyses_input_file <- function(id, filepath_input) {
  
  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_input)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "input_file", "", sep = "/")
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

# Settings File -----------------------------------------------------------------
#' Get analysis settings file
#' 
#' Gets the analysis settings_file contents
#' 
#' @rdname api_get_analyses_settings_file
#' 
#' @param id a unique integer value identifying this analysis.
#' 
#' @return previously posted analysis settings file. 
#' 
#' @importFrom httr GET 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_get_analyses_settings_file <- function(id) {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "settings_file", "", sep = "/")
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

#' Post analysis settings file
#' 
#' Sets the analysis settings_file contents.
#' 
#' @rdname api_post_analyses_settings_file
#' 
#' @param id a unique integer value identifying this analysis.
#' @param filepath_settings path to the settings file.
#' 
#' @return the posted analysis settings file. 
#' 
#' @importFrom httr POST 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' @importFrom httr upload_file
#' 
#' @export
api_post_analyses_settings_file <- function(id, filepath_settings) {
  
  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_settings)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "settings_file", "", sep = "/")
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

# Input errors file ------------------------------------------------------------

#' Get analysis input_errors file
#' 
#' Gets the analysis input_errors_file contents
#' 
#' @rdname api_get_analyses_input_errors_file
#' 
#' @param id a unique integer value identifying this analysis.
#' 
#' @return previously posted analysis input_errors file. 
#' 
#' @importFrom httr GET 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_get_analyses_input_errors_file <- function(id) {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "input_errors_file", "", sep = "/")
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

#' Post analysis input_errors file
#' 
#' Sets the analysis input_errors_file contents.
#' 
#' @rdname api_post_analyses_input_errors_file
#' 
#' @param id a unique integer value identifying this analysis.
#' @param filepath_input_errors path to the input_errors file.
#' 
#' @return the posted analysis input_errors file. 
#' 
#' @importFrom httr POST 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' @importFrom httr upload_file
#' 
#' @export
api_post_analyses_input_errors_file <- function(id, filepath_input_errors) {
  
  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_input_errors)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "input_errors_file", "", sep = "/")
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


# input generation traceback file ----------------------------------------------

#' Get analysis input_generation_traceback file
#' 
#' Gets the analysis input_generation_traceback_file contents
#' 
#' @rdname api_get_analyses_input_generation_traceback_file
#' 
#' @param id a unique integer value identifying this analysis.
#' 
#' @return previously posted analysis input_generation_traceback file. 
#' 
#' @importFrom httr GET 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_get_analyses_input_generation_traceback_file <- function(id) {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "input_generation_traceback_file", "", sep = "/")
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

#' Post analysis input_generation_traceback file
#' 
#' Sets the analysis input_generation_traceback_file contents.
#' 
#' @rdname api_post_analyses_input_generation_traceback_file
#' 
#' @param id a unique integer value identifying this analysis.
#' @param filepath_input_generation_traceback path to the input_generation_traceback file.
#' 
#' @return the posted analysis input_generation_traceback file. 
#' 
#' @importFrom httr POST 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' @importFrom httr upload_file
#' 
#' @export
api_post_analyses_input_generation_traceback_file <- function(id, filepath_input_generation_traceback) {
  
  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_input_generation_traceback)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "input_generation_traceback_file", "", sep = "/")
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

#' Return input_generation_traceback_file Dataframe
#' 
#' @rdname return_input_generation_traceback_file_df
#' 
#' @description Returns a dataframe of input_generation_traceback_file
#' 
#' @param id a unique integer value identifying this analysis.
#' 
#' @return dataframe of input_generation_traceback_file
#' 
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' 
#' @export
return_input_generation_traceback_file_df <- function(id){
  get_input_generation_traceback_file <- api_get_analyses_input_generation_traceback_file(id)
  input_generation_traceback_fileList <- content(get_input_generation_traceback_file$result)
  if (is.null(names(input_generation_traceback_fileList))) {
    input_generation_traceback_file_df <- strsplit(input_generation_traceback_fileList, split = "\n") %>% 
      as.data.frame(stringsAsFactors = FALSE)
    colnames(input_generation_traceback_file_df) <- input_generation_traceback_file_df[1, ]
    input_generation_traceback_file_df <- input_generation_traceback_file_df %>% filter(!! sym(colnames(input_generation_traceback_file_df)) != colnames(input_generation_traceback_file_df) ) 
  } else {
    input_generation_traceback_file_df <- bind_rows(input_generation_traceback_fileList) %>% 
      as.data.frame()
  }
  return(input_generation_traceback_file_df)
}

# output file ------------------------------------------------------------------

#' Get analysis output file
#' 
#' Gets the analysis output_file contents
#' 
#' @rdname api_get_analyses_output_file
#' 
#' @param id a unique integer value identifying this analysis.
#' 
#' @return previously posted analysis output file. 
#' 
#' @importFrom httr GET 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_get_analyses_output_file <- function(id) {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "output_file", "", sep = "/")
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

#' Post analysis output file
#' 
#' Sets the analysis output_file contents.
#' 
#' @rdname api_post_analyses_output_file
#' 
#' @param id a unique integer value identifying this analysis.
#' @param filepath_output path to the output file.
#' 
#' @return the posted analysis output file. 
#' 
#' @importFrom httr POST 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' @importFrom httr upload_file
#' 
#' @export
api_post_analyses_output_file <- function(id, filepath_output) {
  
  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_output)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "output_file", "", sep = "/")
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
# run traceback file -----------------------------------------------------------
#' Get analysis run_traceback file
#' 
#' Gets the analysis run_traceback_file contents
#' 
#' @rdname api_get_analyses_run_traceback_file
#' 
#' @param id a unique integer value identifying this analysis.
#' 
#' @return previously posted analysis run_traceback file. 
#' 
#' @importFrom httr GET 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_get_analyses_run_traceback_file <- function(id) {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "analyses", id, "run_traceback_file", "", sep = "/")
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

#' Post analysis run_traceback file
#' 
#' Sets the analysis run_traceback_file contents.
#' 
#' @rdname api_post_analyses_run_traceback_file
#' 
#' @param id a unique integer value identifying this analysis.
#' @param filepath_run_traceback path to the run_traceback file.
#' 
#' @return the posted analysis run_traceback file. 
#' 
#' @importFrom httr POST 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' @importFrom httr upload_file
#' 
#' @export
api_post_analyses_run_traceback_file <- function(id, filepath_run_traceback) {
  
  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(file = upload_file(filepath_run_traceback)),
    encode = "multipart",
    path = paste(get_version(), "analyses", id, "run_traceback_file", "", sep = "/")
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


#' Return analyses_run_traceback_file Dataframe
#' 
#' @rdname return_analyses_run_traceback_file_df
#' 
#' @description Returns a dataframe of analyses_run_traceback_file
#' 
#' @param id a unique integer value identifying this analysis.
#' 
#' @return dataframe of analyses_run_traceback_file
#' 
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' 
#' @export
return_analyses_run_traceback_file_df <- function(id){
  get_analyses_run_traceback_file <- api_get_analyses_analyses_run_traceback_file(id)
  analyses_run_traceback_fileList <- content(get_analyses_run_traceback_file$result)
  if (is.null(names(analyses_run_traceback_fileList))) {
    analyses_run_traceback_file_df <- strsplit(analyses_run_traceback_fileList, split = "\n") %>% 
      as.data.frame(stringsAsFactors = FALSE)
    colnames(analyses_run_traceback_file_df) <- analyses_run_traceback_file_df[1, ]
    analyses_run_traceback_file_df <- analyses_run_traceback_file_df %>% filter(!! sym(colnames(analyses_run_traceback_file_df)) != colnames(analyses_run_traceback_file_df) ) 
  } else {
    analyses_run_traceback_file_df <- bind_rows(analyses_run_traceback_fileList) %>% 
      as.data.frame()
  }
  return(analyses_run_traceback_file_df)
}
# Portfolio API calls ----------------------------------------------------------

#' Get portfolios
#' 
#' Returns a list of portfolio objects.
#' 
#' @rdname api_get_portfolios
#' 
#' @param name name of the portfolio. Default empty string returns all portfolios.
#' 
#' @return previously posted portfolios. 
#' 
#' @importFrom httr GET 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_get_portfolios <- function(name = "") {
  
  http_type = "application/json"
  
  response <- GET(
    get_url(),
    config = add_headers(
      # Accept = get_http_type(),
      Accept = http_type,
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", "", sep = "/"),
    query = list(name = name)
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

#' Get portfolios location file
#' 
#' Gets the portfolios location_file contents
#' 
#' @rdname api_get_portfolios_location_file
#' 
#' @param id a unique integer value identifying this analysis.
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
      Accept = http_type,
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

# R functions calling Portfolio API functions and manipulating the output ------

#' Return Portfolios Dataframe
#' 
#' @rdname return_portfolios_df
#' 
#' @description Returns a dataframe of portfolios
#' 
#' @param name name of the portfolio.
#' 
#' @return dataframe of previously posted portfolios. Default empty string returns all portfolios.
#' 
#' @importFrom dplyr bind_rows
#' 
#' @export
return_portfolios_df <- function(name = ""){
  get_portfolios <- api_get_portfolios(name)
  portfoliosList <- content(get_portfolios$result)
  portfolios_df <- bind_rows(portfoliosList) %>% #do.call("rbind", portfoliosList) %>% 
    as.data.frame()
  return(portfolios_df)
}

#' Return Portfolios Data fot DT
#' 
#' @rdname return_tbl_portfoliosData
#' 
#' @description Returns a dataframe of portfolios ready for being rendered as a data table
#' 
#' @param name name of the portfolio.
#' 
#' @return dataframe of previously posted portfolios. Default empty string returns all portfolios.
#' 
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' 
#' @export
return_tbl_portfoliosData <- function(name = ""){
  tbl_portfoliosData <- return_portfolios_df(name) %>%
    select(-contains("file") ) %>% 
    as.data.frame()
  tbl_portfoliosData[[tbl_portfoliosData.PortfolioCreated]] <- as.POSIXct(tbl_portfoliosData[[tbl_portfoliosData.PortfolioCreated]], format = "%d-%m-%YT%H:%M:%S")
  tbl_portfoliosData[[tbl_portfoliosData.PortfolioModified]] <- as.POSIXct(tbl_portfoliosData[[tbl_portfoliosData.PortfolioModified]], format = "%d-%m-%YT%H:%M:%S")
  idx <- unlist(tbl_portfoliosData[[tbl_portfoliosData.PortfolioID]])
  numpf <- length(idx)
  status <- data.frame(Status = rep(status_code_notfound, numpf))
  for (i in seq(numpf) ){
    id <- as.integer(idx[i])
    get_portfolios_location_file <- api_get_portfolios_location_file(id)
    status[i, "Status"] <- toString(get_portfolios_location_file$result$status_code)
  }
  tbl_portfoliosData <- cbind(tbl_portfoliosData, status) %>%
    as.data.frame()
  return(tbl_portfoliosData)
}
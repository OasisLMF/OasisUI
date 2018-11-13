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
      Accept = get_http_type(),
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

#' Get portfolios id
#' 
#' Returns the specific analysis entry.
#' 
#' @rdname api_get_portfolios_id
#' 
#' @param id a unique integer value identifying this analysis.
#' 
#' @return previously posted portfolios id. 
#' 
#' @importFrom httr GET
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_get_portfolios_id <- function(id) {
  
  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", id, "", sep = "/")
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
  idx <- tbl_portfoliosData[[tbl_portfoliosData.PortfolioID]]
  numpf <- length(idx)
  status <- data.frame(Status = rep(status_code_notfound, numpf))
  for (i in seq(numpf) ) {
    id <- as.integer(idx[i])
    get_portfolios_location_file <- api_get_portfolios_location_file(id)
    status[i, "Status"] <- toString(get_portfolios_location_file$result$status_code)
    tbl_portfoliosData[i, tbl_portfoliosData.PortfolioCreated] <- toString(as.POSIXct(tbl_portfoliosData[i, tbl_portfoliosData.PortfolioCreated] , format = "%d-%m-%YT%H:%M:%S"))
    tbl_portfoliosData[i, tbl_portfoliosData.PortfolioModified] <- toString(as.POSIXct(tbl_portfoliosData[i, tbl_portfoliosData.PortfolioModified], format = "%d-%m-%YT%H:%M:%S"))
  }
  tbl_portfoliosData <- cbind(tbl_portfoliosData, status) %>%
    as.data.frame()
  return(tbl_portfoliosData)
}

#' Return Details Portfolio id Dataframe
#' 
#' @rdname return_portfolio_details_df
#' 
#' @description Returns a dataframe of portfolio's details
#' 
#' @param id id of the portfolio.
#' 
#' @return dataframe of details of previously posted portfolio
#' 
#' @importFrom dplyr bind_rows
#' 
#' @export
return_portfolio_details_df <- function(id){
  get_portfolio_details <- api_get_portfolios_id(id)
  portfolioDetailsList <- content(get_portfolio_details$result)
  portfolio_details_df <- bind_rows(portfolioDetailsList) %>% #do.call("rbind", portfoliosList) %>% 
    as.data.frame()
  return(portfolio_details_df)
}

#' Return Portfolio Details fot DT
#' 
#' @rdname return_tbl_portfolioDetails
#' 
#' @description Returns a dataframe of portfolio's details ready for being rendered as a data table
#' 
#' @param id id of the portfolio.
#' 
#' @return dataframe of details of previously posted portfolio.
#' 
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#' @importFrom tidyr gather
#' 
#' @export
return_tbl_portfolioDetails <- function(id){
  
  #Help function to replace variable with icon
  .replacewithIcon <- function(var){
    var <- case_when(var %in% status_code_exist ~ StatusCompleted,
                     var %in% status_code_notfound ~ StatusFailed,
                     var %notin% c(status_code_notfound, status_code_exist) ~ StatusProcessing)
    return(var)
  }
  
  tbl_portfolioDetails <- return_portfolio_details_df(id) %>%
    as.data.frame()
  #format date
  tbl_portfolioDetails[[tbl_portfolioDetails.PortfolioCreated ]] <- toString(as.POSIXct(tbl_portfolioDetails[[tbl_portfolioDetails.PortfolioCreated ]], format = "%d-%m-%YT%H:%M:%S"))
  tbl_portfolioDetails[[tbl_portfolioDetails.PortfolioModified]] <- toString(as.POSIXct(tbl_portfolioDetails[[tbl_portfolioDetails.PortfolioModified]], format = "%d-%m-%YT%H:%M:%S"))
  #Replace files with Icons
    #Location file
    get_portfolios_location_file <- api_get_portfolios_location_file(id) 
    tbl_portfolioDetails[[tbl_portfolioDetails.PortfolioLoc]] <- toString(get_portfolios_location_file$result$status_code) %>%
      .replacewithIcon()
    #Account file
    get_portfolios_accounts_file <- api_get_portfolios_accounts_file(id) 
    tbl_portfolioDetails[[tbl_portfolioDetails.PortfolioAcc]] <- toString(get_portfolios_accounts_file$result$status_code) %>%
      .replacewithIcon()
    #Reinsurance Info file
    get_portfolios_reinsurance_info_file <- api_get_portfolios_reinsurance_info_file(id) 
    tbl_portfolioDetails[[tbl_portfolioDetails.PortfolioRIinfo]] <- toString(get_portfolios_reinsurance_info_file$result$status_code) %>%
      .replacewithIcon()
    #Reinsurance Source file
    get_portfolios_reinsurance_source_file <- api_get_portfolios_reinsurance_source_file(id) 
    tbl_portfolioDetails[[tbl_portfolioDetails.PortfolioRIsource]] <- toString(get_portfolios_reinsurance_source_file$result$status_code) %>%
      .replacewithIcon()
  # reshape df
  tbl_portfolioDetails <- gather(tbl_portfolioDetails,  key = "fields", value = "value") %>%
    as.data.frame()
  return(tbl_portfolioDetails)
}

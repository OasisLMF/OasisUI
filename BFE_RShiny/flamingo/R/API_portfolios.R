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

#' Post portfolios
#' 
#' Creates a portfolio based on the input data.
#' 
#' @rdname api_post_portfolios
#' 
#' @param name name of the portfolio.
#' 
#' @return the posted portfolio. 
#' 
#' @importFrom httr POST 
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_post_portfolios <- function(name) {
  
  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())#,
    ),
    body = list(name = name),
    encode = "json",
    path = paste(get_version(), "portfolios", "", sep = "/")
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

#' Put portfolios id
#' 
#' Updates the specified analysis.
#' 
#' @rdname api_put_portfolios_id
#' 
#' @param name name of the portfolio.
#' @param id a unique integer value identifying this portfolio.
#' 
#' @return the updated analysis. 
#' 
#' @importFrom httr PUT
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_put_portfolios_id <- function(id, name) {
  
  response <- PUT(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())#,
    ),
    body = list(name = name),
    encode = "json",
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


#' Get portfolios info by id
#' 
#' Returns the specific portfolio entry.
#' 
#' @rdname api_get_portfolios_id
#' 
#' @param id a unique integer value identifying this portfolio
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

#' Delete portfolios by id
#' 
#' @rdname api_delete_portfolios_id
#' 
#' @param id a unique integer value identifying this portfolio.
#' 
#' @importFrom httr DELETE
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_delete_portfolios_id <- function(id) {
  
  response <- DELETE(
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


#' Post portfolios create analysis
#' 
#' Creates an analysis object from the portfolio.
#' 
#' @rdname api_post_portfolios_create_analysis
#' 
#' @param name name of the portfolio.
#' @param id a unique integer value identifying this analysis.
#' @param model id of model
#' 
#' @return the posted portfolio analysis created. 
#' 
#' @importFrom httr POST
#' @importFrom httr add_headers 
#' @importFrom httr warn_for_status 
#' @importFrom httr http_status
#' 
#' @export
api_post_portfolios_create_analysis <- function(id, name, model) {
  
  response <- POST(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    body = list(name = name, model = model),
    encode = "json",
    path = paste(get_version(), "portfolios", id, "create_analysis", "", sep = "/")
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
#' @importFrom httr content
#' 
#' @export
return_portfolios_df <- function(name = ""){
  
  .showname <- function(x){
    if (length(x) > 1) {
      y <- x$name
    } else if (length(x) == 0) {
      y <- "Not Available"
    } else {
      y <- x
    }
    return(y)
  }
  
  get_portfolios <- api_get_portfolios(name)
  portfoliosList <- content(get_portfolios$result)
  
  portfoliosList <- lapply(portfoliosList,function(x){lapply(x, .showname)})
  
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
#' @importFrom dplyr arrange
#' @importFrom dplyr sym
#' @importFrom dplyr desc
#' 
#' @export
return_tbl_portfoliosData <- function(name = ""){
  
  tbl_portfoliosData <- return_portfolios_df(name)
  if (nrow(tbl_portfoliosData) > 0) {
    idx <- tbl_portfoliosData[[tbl_portfoliosData.PortfolioID]]
    numpf <- length(idx)
    for (i in seq(numpf) ) {
      tbl_portfoliosData[i, tbl_portfoliosData.PortfolioCreated] <- toString(as.POSIXct(tbl_portfoliosData[i, tbl_portfoliosData.PortfolioCreated] , format = "%d-%m-%YT%H:%M:%S"))
      tbl_portfoliosData[i, tbl_portfoliosData.PortfolioModified] <- toString(as.POSIXct(tbl_portfoliosData[i, tbl_portfoliosData.PortfolioModified] , format = "%d-%m-%YT%H:%M:%S"))
    }
    tbl_portfoliosData <- cbind(tbl_portfoliosData, data.frame(status = ifelse(tbl_portfoliosData$location_file == "Not Available", StatusProcessing, StatusCompleted)))
    
    tbl_portfoliosDetailsStatus <- tbl_portfoliosData  %>%
      select(-contains("file") ) %>% 
      arrange(desc(!! sym(tbl_portfoliosData.PortfolioID))) %>%
      as.data.frame()

  } else {
    tbl_portfoliosDetailsStatus <- NULL
  }
  
  return(tbl_portfoliosDetailsStatus)
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
#' @importFrom httr content
#' 
#' @export
return_portfolio_details_df <- function(id){
  
  .showname <- function(x){
    if (length(x) > 1) {
      y <- x$name
    } else if (length(x) == 0) {
      y <- "Not Available"
    } else {
      y <- x
    }
    return(y)
  }
  
  get_portfolio_details <- api_get_portfolios_id(id)
  portfolioDetailsList <- content(get_portfolio_details$result)
  
  portfolioDetailsList <- lapply(portfolioDetailsList, .showname)
  
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
#' @importFrom dplyr contains
#' @importFrom tidyr gather
#' 
#' @export
return_tbl_portfolioDetails <- function(id){
  
  tbl_portfolioDetails <- return_portfolio_details_df(id) %>%
    select(contains("file") ) %>% 
    as.data.frame()

  # reshape df
  tbl_portfolioDetails <- gather(tbl_portfolioDetails,  key = "files", value = "name")
  # tbl_portfolioDetails <- tbl_portfolioDetails %>% mutate(status = ifelse( tbl_portfolioDetails$name == "Not Available", StatusProcessing, StatusCompleted)) %>%
  tbl_portfolioDetails <-tbl_portfolioDetails  %>%
    as.data.frame()
  
  return(tbl_portfolioDetails)
}

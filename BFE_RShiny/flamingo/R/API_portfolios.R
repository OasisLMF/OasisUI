# Portfolio API calls ----------------------------------------------------------

#' Get portfolios
#'
#' Returns a list of portfolio objects.
#'
#' @rdname api_get_portfolios
#'
#' @param name Name of the portfolio. Default empty string returns all portfolios.
#'
#' @return Previously posted portfolios.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#'
#' @export
api_get_portfolios <- function(name = "") {

  response <- GET(
    get_url(),
    config = add_headers(
      Accept = get_http_type(),
      Authorization = sprintf("Bearer %s", get_token())
    ),
    path = paste(get_version(), "portfolios", "", sep = "/"),
    query = list(name = name)
  )

  api_handle_response(response)
}

#' Post portfolios
#'
#' Creates a portfolio based on the input data.
#'
#' @rdname api_post_portfolios
#'
#' @param name Name of the portfolio.
#'
#' @return The posted portfolio.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
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

  api_handle_response(response)
}

#' Put portfolios id
#'
#' Updates the specified analysis.
#'
#' @rdname api_put_portfolios_id
#'
#' @param name Name of the portfolio.
#' @param id A unique integer value identifying this portfolio.
#'
#' @return The updated analysis.
#'
#' @importFrom httr PUT
#' @importFrom httr add_headers
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

  api_handle_response(response)
}

#' Get portfolios info by id
#'
#' Returns the specific portfolio entry.
#'
#' @rdname api_get_portfolios_id
#'
#' @param id A unique integer value identifying this portfolio.
#'
#' @return Previously posted portfolios id.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
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

  api_handle_response(response)
}

#' Delete portfolios by id
#'
#' @rdname api_delete_portfolios_id
#'
#' @param id A unique integer value identifying this portfolio.
#'
#' @importFrom httr DELETE
#' @importFrom httr add_headers
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

  api_handle_response(response)
}

#' Post portfolios create analysis
#'
#' Creates an analysis object from the portfolio.
#'
#' @rdname api_post_portfolios_create_analysis
#'
#' @param name Name of the portfolio.
#' @param id A unique integer value identifying this analysis.
#' @param model Id of the model.
#'
#' @return The posted portfolio analysis created.
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
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

  api_handle_response(response)
}

# R functions calling Portfolio API functions and manipulating the output ------

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
  
  tbl_portfoliosData <- return_df(api_get_portfolios, name)
  
  if (!is.null(tbl_portfoliosData) && nrow(tbl_portfoliosData) > 0 && is.null(tbl_portfoliosData$detail)) {
    
    tbl_portfoliosData <- cbind(tbl_portfoliosData, data.frame(status = ifelse(tbl_portfoliosData$location_file == "Not Available", Status$Processing, Status$Completed)))
    
    tbl_portfoliosData <- convert_created_modified(tbl_portfoliosData)
    
    tbl_portfoliosDetailsStatus <- tbl_portfoliosData  %>%
      select(-contains("file") ) %>% 
      arrange(desc(!! sym(tbl_portfoliosDataNames$id))) %>%
      as.data.frame()

  } else {
    tbl_portfoliosDetailsStatus <- NULL
  }
  
  return(tbl_portfoliosDetailsStatus)
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
  
  tbl_portfolioDetails <- return_df(api_get_portfolios_id, id)
  
  if (!is.null(tbl_portfolioDetails) && is.null(tbl_portfolioDetails$detail)) {
    
    tbl_portfolioDetails <- tbl_portfolioDetails %>%
      select(contains("file") ) %>%
      as.data.frame()
    
    # reshape df
    tbl_portfolioDetails <- gather(tbl_portfolioDetails,  key = "files", value = "name")
    # tbl_portfolioDetails <- tbl_portfolioDetails %>% mutate(status = ifelse( tbl_portfolioDetails$name == "Not Available", Status$Processing, Status$Completed)) %>%
    tbl_portfolioDetails <- tbl_portfolioDetails  %>%
      as.data.frame()
  }
  
  return(tbl_portfolioDetails)
}

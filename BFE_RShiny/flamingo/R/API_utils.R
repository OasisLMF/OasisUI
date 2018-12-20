#' return element based on length of input
#'
#' @rdname showname
#'
#' @description Returns the element name of the input x if x has legth > 1, the elemnt x itself if it has length = 1, and "Not available" if if x has legth = 0
#'
#' @param x list, NULL or string
#'
#' @return string
#'
#' @export
showname <- function(x){
  if (length(x) > 1) {
    y <- x$name
  } else if (length(x) == 0) {
    y <- "Not Available"
  } else {
    y <- x
  }
  return(y)
}



#' Return Dataframe from API response
#'
#' @rdname return_df
#'
#' @description Returns a dataframe of the API response
#'
#' @param api_query function representing the API query
#' @param api_param parameter for the api_query
#'
#' @return dataframe of the API response
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#'
#' @export
return_df <- function(api_query, api_param = ""){
  
  get_response <- api_query(api_param)
  responseList <- content(get_response$result)
  
  if (length(responseList[[1]]) > 1 ) {
    responseList <- lapply(responseList,function(x){lapply(x, showname)})
  } else {
    responseList <- lapply(responseList, showname)
  }

  response_df <- bind_rows(responseList) %>% #do.call("rbind", portfoliosList) %>% 
    as.data.frame()
  return(response_df)
}


#' Return file as Dataframe
#'
#' @rdname return_file_df
#'
#' @description Returns a dataframe of a file downloaded from API
#'
#' @param api_query function representing the API query
#' @param api_param parameter for the api_query
#'
#' @return Dataframe of a file downloaded from API
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
  return(file_df)
}


#' Get Geo Details
#' @description this function can be used to query google servers for
#' geo details.
#' @param address server address
#' @return \code{data.frame} with details. If no match is found, the result
#' is a \code{data.frame} of \code{NA} values.\@importFrom ggmap geocode
#' @export
getGeoDetails <- function(address) {   
  geo_reply <- geocode(address, output = "all", messaging = TRUE,
      override_limit = TRUE)
  
  answer <- data.frame(number = NA, street = NA, postcode = NA, lat = NA,
      long = NA, formatted_address = NA, accuracy = NA, status = NA)
  
  if (geo_reply$status != "OK") {
    return(answer)
  }
  
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0) {
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  answer$status <- paste(geo_reply$status)
  return(answer)
} 

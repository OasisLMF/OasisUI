#' Capitalize first letter of all words in a string
#'
#' @rdname capitalize_first_letter
#'
#' @description Returns a string with all words of input string have the first letter capitalized.
#'
#' @param x string
#'
#' @return string
#'
#' @export

capitalize_first_letter <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}


#' Capitalize names of a dataframe
#'
#' @rdname capitalize_names_df
#'
#' @description Returns a dataframe with names with the first letterof all words capitalized.
#'
#' @param df dataframe
#'
#' @return df
#'
#' @export

capitalize_names_df <- function(df) {
  names(df) <- sapply(
    sapply(names(df), gsub, pattern = "_", replacement = " "),
    capitalize_first_letter)
  #replace Id with ID
  names(df)[names(df) == "Id"] <- "ID"
  df
}

#' Nothing to show in table
#'
#' @rdname nothingToShowTable
#'
#' @description Shows message in case table is table.
#'
#' @param contentMessage Message to be displayed.
#'
#' @export
nothingToShowTable <- function(contentMessage){
  datatable(
    data.frame(content = contentMessage),
    class = "flamingo-table display",
    selection = "none",
    rownames = FALSE,
    #filter = 'bottom',
    colnames = c(""),
    escape = FALSE,
    options = list(searchHighlight = TRUE)
  )
}

#' Get table options.
#'
#' @rdname getTableOptions
#'
#' @description Returns option list for datatable.
#'
#' @param scrollX Param to allow scrollX.
#' @param maxrowsperpage Maximum number of rows to display per page.
#' @param filter Show or hide filter.
#' @param escape Param to avoid escape row.
#'
#' @export
getTableOptions <- function(  scrollX = FALSE,
                              maxrowsperpage = 5,
                              filter = TRUE,
                              escape = TRUE) {

  options <- list(
    search = list(caseInsensitive = TRUE),
    searchHighlight = TRUE,
    #columnDefs = list(list(visible = FALSE, targets = c(0,5,6))),
    processing = 0,
    scrollX = scrollX,
    pageLength = maxrowsperpage,
    #autoWidth = TRUE,
    columnDefs = list(list(visible = FALSE, targets = 0))
  )
  if (filter) {
    options$dom <- 'ft'
    options$search <- list(caseInsensitive = TRUE)
    options$searchHighlight <- TRUE
  } else {
    options$dom <- 't'
  }
  if (!escape) {
    options$preDrawCallback <- JS('function() { Shiny.unbindAll(this.api().table().node()); }')
    options$drawCallback <- JS('function() { Shiny.bindAll(this.api().table().node()); } ')
  }
  return(options)
}

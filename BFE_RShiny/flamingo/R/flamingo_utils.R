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

#' Pipe operator
#'
#' @rdname pipe
#'
#' @description pipe operator imported from dplyr to avoid unncessary roxygen tags
#' 
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#'
#' @export
#'
#' @md
'%>%' <- dplyr::'%>%'

#' Negate in operator
#'
#' @rdname negate
#'
#' @description negate in operator
#' 
#' @param ... extra arguments to [Negate()]
#'
#' @export
#'
#' @md
'%notin%' <- Negate('%in%')
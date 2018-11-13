#' Pipe operator
#'
#' @rdname pipe
#'
#' @description pipe operator imported from dplyr to avoid unncessary roxygen tags
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
#' @export
#'
#' @md
'%notin%' <- Negate('%in%')
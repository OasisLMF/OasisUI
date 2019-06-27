#' Get API env vars
#'
#' Fetches values of environment variables and combines them conveniently in a
#' list.
#'
#' @rdname APIgetenv
#'
#' @param ... Names of environment variables. If passed as named arguments, the
#'   returned list will retain the same names.
#'
#' @return List of environment variables' values.
#'
#' @export
APIgetenv <- function(...) {
  lapply(list(...), Sys.getenv)
}

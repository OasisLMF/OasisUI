#' Return element based on length of input
#'
#' @rdname showname
#'
#' @description Returns the element name of the input x if x has length > 1, the
#'   element x itself if it has length = 1, and "Not available" if x has length
#'   = 0.
#'
#' @param x List, NULL or string.
#'
#' @return String.
#'
#' @export
showname <- function(x) {
  if (length(x) > 1) {
    y <- x$name
  } else if (length(x) == 0) {
    y <- "Not Available"
  } else {
    y <- x
  }
  y
}

#' Format-conversion for timestamps in tables
#'
#' @rdname convert_created_modified
#'
#' @description Replaces created and modified columns with different date format.
#'
#' @param tbl_obj Dataframe to convert.
#'
#' @return Dataframe with converted columns.
#'
#' @export
convert_created_modified <- function(tbl_obj) {
  tbl_obj_names <- names(tbl_obj)
  numpf <- nrow(tbl_obj)
  for (i in seq(numpf)) {
    tbl_obj[i, "created"] <- toString(as.POSIXct(tbl_obj[i, "created"], format = "%Y-%m-%dT%H:%M:%S"))
    tbl_obj[i, "modified"] <- toString(as.POSIXct(tbl_obj[i, "modified"], format = "%Y-%m-%dT%H:%M:%S"))
    # tasks started and finished available only for analyses table
    if (length(tbl_obj[i, "task_started"]) != 0) {
      tbl_obj[i, "task_started"] <- toString(as.POSIXct(tbl_obj[i, "task_started"], format = "%Y-%m-%dT%H:%M:%S"))
      tbl_obj[i, "task_finished"] <- toString(as.POSIXct(tbl_obj[i, "task_finished"], format = "%Y-%m-%dT%H:%M:%S"))
    }
  }
  tbl_obj
}

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

#' add_commas
#'
#' @description Inserts commas at thousands.
#'
#' @param value Value to be re-formatted.
#'
#' @return Checked uploaded locations (added peril_id).
#'
#' @export
add_commas <- function(value) {
  format(value, big.mark = ",", scientific = FALSE)
}

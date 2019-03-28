# Exposure Validation Module ---------------------------------------------------

# UI ---------------------------------------------------------------------------
#' exposurevalidationUI
#' @rdname exposurevalidation
#'
#' @description UI/View for exposure validation of an analysis.
#'
#' @return List of tags.
#'
#' @export
exposurevalidationUI <- function(id) {

  ns <- NS(id)

  tagList(
    flamingoRefreshButton(ns("abuttonexposurerefresh"))
  )
}


# Server -----------------------------------------------------------------------

#' exposurevalidation
#'
#' @rdname exposurevalidation
#'
#' @description Server logic for exposure validation of an analysis.
#'
#' @export
exposurevalidation <- function(input,
                               output,
                               session) {

  ns <- session$ns

  onclick("abuttonexposurerefresh", {

  })

}

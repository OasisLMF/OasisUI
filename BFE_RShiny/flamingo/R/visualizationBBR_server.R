# visualization Batch Run Browse Module Server ---------------------------------

#' visualizationBBR
#'
#' @rdname visualizationBBR
#'
#' @description Server logic for batchbrowse run page.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @export
visualizationBBR <- function(input, output, session, dbSettings, apiSettings,
                              userId, active = reactive(TRUE), logMessage = message,
                              reloadMillis = 10000) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------

  navigation_state <- reactiveNavigation()

  result <- reactiveValues() # placeholder

  # Model Outout ---------------------------------------------------------------

  moduleOutput <- c(
    outputNavigation(navigation_state),
    list() # placeholder
  )

  moduleOutput

}

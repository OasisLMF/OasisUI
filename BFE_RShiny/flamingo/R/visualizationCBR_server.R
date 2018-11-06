#' Compare Runs Definition Module Server ---------------------------------------

#' visualizationCBR
#'
#' @rdname visualizationCBR
#'
#' @description Server logic for comparing runs run page.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @export
visualizationCBR <- function(input, output, session, dbSettings, apiSettings,
                             userId, active = reactive(TRUE), logMessage = message,
                             reloadMillis = 10000) {

  ns <- session$ns

  # Reactive Values and parameters ------------------------------------------

  navigation_state <- reactiveNavigation()

  result <- reactiveValues() # placeholder

  # Model Outout ---------------------------------------------------------------

  moduleOutput <- c(
    outputNavigation(navigation_state),
    list() # placeholder
  )

  moduleOutput

}

#' Compare Runs Definition Module
#' @rdname visualizationCBR
#' @description Server logic to compare runs
#' @inheritParams flamingoModule
#' @return For \code{visualizationCBR()}, list of reactives.
#' @template return-outputNavigation
#' @export
visualizationCBR <- function(input, output, session, dbSettings, apiSettings,
                             userId, active = reactive(TRUE), logMessage = message) {

  ns <- session$ns

  # Reactive Values and parameters ------------------------------------------

  navigation_state <- reactiveNavigation()

  result <- reactiveValues() # placeholder

  # Model Outout ------------------------------------------------------------

  moduleOutput <- c(
    outputNavigation(navigation_state),
    list() # placeholder
  )

  moduleOutput

}

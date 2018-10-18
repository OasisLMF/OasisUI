#' Compare Runs Definition Module
#' @rdname visualizationCBR
#' @description Server logic to compare runs
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @return For \code{visualizationCBR()}, list of reactives.
#' @template return-outputNavigation
#' @export
visualizationCBR <- function(input, output, session, dbSettings, apiSettings,
                             userId, active = reactive(TRUE), logMessage = message,
                             reloadMillis = 10000) {

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

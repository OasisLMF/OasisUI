#' Batch Browse Definition Module
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @return For \code{visualizationBBR()}, list of reactives.
#' @template return-outputNavigation
#' @rdname visualizationBBR
#' @export
visualizationBBR <- function(input, output, session, dbSettings, apiSettings,
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

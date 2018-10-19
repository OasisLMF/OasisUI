#' Batch Browse Definition Module
#' @rdname visualizationBBR
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @return For \code{visualizationBBR()}, list of reactives.
#' @template return-outputNavigation
#' @export
visualizationBBR <- function(input, output, session, dbSettings, apiSettings,
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

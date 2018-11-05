#' programmeDefinitionBatch_server
#'
#' @rdname programmeDefinitionBatch
#'
#' @description Server logic to define a programme.
#'
#' @template return-outputNavigation
#'
#' @inheritParams flamingoModule
#' @inheritParams landingPage
#'
#' @return For \code{programmeDefinitionBatch()}, list of reactives.
#'
#' @export
programmeDefinitionBatch <- function(input, output, session, dbSettings,
                                     apiSettings, userId, active = reactive(TRUE),
                                     logMessage = message, reloadMillis = 10000) {

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

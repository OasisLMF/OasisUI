#' Single Browse Definition Module
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @return For \code{visualizationSBR()}, list of reactives.
#' @template return-outputNavigation
#' @rdname visualizationSBR
#' @export
visualizationSBR <- function(input, output, session, dbSettings,
                                   apiSettings, userId,
                                   runIdList, preselRunId, processRunId,
                                   active = reactive(TRUE), logMessage = message,
                                   reloadMillis = 10000) {

  ns <- session$ns

  # Reactive Values and parameters ------------------------------------------

  #navigation_state <- reactiveNavigation()

  result <- reactiveValues(

  )

  # Run identification -----------------------------------------------------

  # Additional plot --------------------------------------------------------

  # Plots ------------------------------------------------------------------

  # Go to Configure Output button -----------------------------------------------

  invisible()

}

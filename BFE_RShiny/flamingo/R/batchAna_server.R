#' batchAna
#'
#' @rdname batchAna
#'
#' @description Server logic to define a batch analysis.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @export
batchAna <- function(input, output, session,
                    active = reactive(TRUE),
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

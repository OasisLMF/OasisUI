#' browseprogrammes Module
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater};
#' @return list of reactives:
#' \itemize{
#' 		\item{\code{navigate}}{reactive yielding navigation}
#' 		\item{\code{progOasisId}}{selected ProgOasis Id}
#' }
#' @rdname browse
#' @importFrom shinyjs show hide enable disable
#' @importFrom DT renderDataTable
#' @importFrom dplyr mutate
#' @export
browseprogrammes <- function(input, output, session, dbSettings,
                                apiSettings, userId, active = reactive(TRUE), logMessage = message,
                                reloadMillis = 10000) {

  ns <- session$ns

  invisible()

}

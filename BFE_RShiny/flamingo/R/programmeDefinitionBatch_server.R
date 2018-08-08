#' Batch Programme Definition Module
#' @description Server logic to define a programme
#' @inheritParams flamingoModule
#' @param reloadMillis amount of time to wait between table updates;
#' see \link{invalidateLater}; 
#' @return list of reactives:
#' @rdname programmeDefinitionBatch
#' @importFrom shinyjs show hide enable disable
#' @importFrom DT renderDataTable
#' @importFrom dplyr mutate
#' @export
programmeDefinitionBatch <- function(input, output, session, dbSettings,
                                     apiSettings, userId, active = reactive(TRUE), logMessage = message,
                                     preselRunId = reactive(-1),
                                     preselProcId = reactive(-1),
                                     reloadMillis = 10000) {
  
  ns <- session$ns
  
  # Reactive Values and parameters ------------------------------------------
  
  result <- reactiveValues(
    #flag to navigate to different pages
    navigate = NULL
  )
  
  # Model Outout ------------------------------------------------------------
  
  moduleOutput <- list(
    navigate = reactive(result$navigate)
  )
  
  return(moduleOutput)
  
}
# defineID Module -----------------------------------------------------------

# Server -----------------------------------------------------------------------

#' defineID Module
#' @rdname defineID
#' @description Server logic for defining one run ID
#' @inheritParams flamingoModule
#' @param runIdList list of runs and their status
#' @param preselectedRunId reactive string expression for reselected run id from landingpage
#' @return reactive for runID selected 
#' @importFrom dplyr mutate select contains filter
#' @export
defineID <- function(input, output, session, dbSettings,
                     apiSettings, userId,
                     runIdList = reactive(c(-1)),
                     preselectedRunId = reactive(-1),
                     nruns = 1, 
                     batch = FALSE,
                     active = reactive(TRUE), logMessage = message) {
  
  ns <- session$ns
  
  # list of sub-modules
  sub_modules <- list()
  
  # width
  w <- floor(12/(nruns + 1))
  
  # Reactive Values and parameters ---------------------------------------------
  result <- reactiveValues(
    selectRunID = NULL
  )
  
  # RunIDS ---------------------------------------------------------------------
  
  lapply(seq(nruns), function(i){
    
  })
  
  sub_modules$oneID <- callModule(
    oneID,
    id = "oneID",
    dbSettings = dbSettings,
    apiSettings = apiSettings,
    userId = userId,
    runIdList = reactive(c(-1)),
    preselectedRunId = reactive(-1),
    w = 6,
    i = 1, 
    batch = FALSE,
    logMessage = logMessage)
  
  # Module Outout --------------------------------------------------------------
  moduleOutput <- c(
    list(
      selectRunID = reactive({result$selectRunID})
    )
  )
  
}


# define One ID module ---------------------------------------------------------
# UI --------------------------------------------------------------------------- 
#' @title defineIDUI
#' @rdname defineIDUI
#' @inheritParams flamingoModuleUI
#' @param w width of the coulmn
#' @param i counter
#' @importFrom bsplus bs_embed_tooltip
#' @export
defineIDUI <- function(id, w, batch = FALSE){
  ns <- NS(id)
  
  labelrun <- "Run ID"
  if (batch) {
    labelrun <- "Batch ID"
  }
  
  column(w,
         selectInput(inputId =  ns(paste0("selectRunID")), label = labelrun, choices = "", selected = NULL) %>%
           bs_embed_tooltip(title = browse_programmes$selectRunID, placement = "right"))
  
}

# Server -----------------------------------------------------------------------
#' define oneID Module
#' @rdname defineID
#' @description Server logic for defining one run ID
#' @inheritParams flamingoModule
#' @param runIdList list of runs and their status
#' @param preselectedRunId reactive string expression for reselected run id from landingpage
#' @return reactive for runID selected 
#' @importFrom dplyr mutate select contains filter
#' @export
defineID <- function(input, output, session, 
                     runIdList = reactive(c(-1)),
                     preselectedRunId = reactive(-1),
                     logMessage = message) {
  
  ns <- session$ns
  
  # Reactive Values and parameters ---------------------------------------------
  result <- reactiveValues(
    selectRunID = ""
  )
  
  # Update RunID----------------------------------------------------------------
  
  observeEvent(preselectedRunId(), {
    index <- match(c(preselectedRunId()), runIdList()$RunID)
    if (!is.null(index) & !is.na(index)) {
      updateSelectInput(session, inputId = "selectRunID", choices = runIdList()$RunID, selected = runIdList()$RunID[index])
    }
  })
  
  observeEvent(input[["selectRunID"]], ignoreInit = TRUE, {
    result$selectRunID <- input[["selectRunID"]]
  })
  
  
  # Module Outout --------------------------------------------------------------
  
  selectRunID <- reactive({
    if ((is.null(result$selectRunID) | is.na(result$selectRunID))) {
      result$selectRunID <-  ""
    } else {
      result$selectRunID <- result$selectRunID
    }
  })
  
  moduleOutput <- c(
    list(
      selectRunID = reactive(selectRunID())
    )
  )
  
}

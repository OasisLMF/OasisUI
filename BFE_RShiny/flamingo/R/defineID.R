# define One ID module ---------------------------------------------------------
# UI --------------------------------------------------------------------------- 

#' defineIDUI
#' 
#' @rdname defineID
#' 
#' @description UI/View for defining one run ID
#' 
#' @template params-module-ui
#' 
#' @param w width of the coulmn.
#' 
#' @param batch flag indicating if it is a batch or a simple run.
#' 
#' @return List of tags.
#' 
#' @importFrom bsplus bs_embed_tooltip
#' 
#' @export
defineIDUI <- function(id, w, batch = FALSE){
  ns <- NS(id)
  
  labelrun <- "Run ID"
  if (batch) {
    labelrun <- "Batch ID"
  }
  
  column(w,
         actionButton(ns(paste0("chooseRunID")), label = NULL, icon = icon("list-alt"), 
                      style = " color: rgb(71, 73, 73);
                               background-color: white;
                               padding: 0px;
                               font-size: 24px;
                               background-image: none;
                               border: none;
                                ") %>%
           bs_embed_tooltip(title = browse_programmes$selectRunID, placement = "right"),
         div(textOutput(ns("selectRunInfo1"), inline = TRUE), style = "font-weight:bold; font-color: #2d2d2d; display:inline;"),
         div(textOutput(ns("selectRunInfo2"), inline = TRUE), style = "font-weight:bold; font-color: #2d2d2d; display:inline; 
                                                                       padding:10px; margin: 5px; border-style: ridge;"),
         style = "display:inline;")
  
}

# Server -----------------------------------------------------------------------

#' defineID
#' 
#' @rdname defineID
#' 
#' @description Server logic for defining one run ID.
#' 
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#' 
#' @param preselectedRunId reactive string expression for reselected run id from \link{landingpage}
#' 
#' @param batch Flag indicating if it is a batch or a simple run.
#' 
#' @return selectRunID reactive for runID selected.
#' 
#' @export
defineID <- function(input, output, session, 
                     dbSettings, userId, 
                     preselectedRunId = reactive(-1),
                     batch = FALSE,
                     logMessage = message) {
  
  ns <- session$ns
  
  # Reactive Values and parameters ---------------------------------------------
  result <- reactiveValues(
    inbox = NULL,
    selectRunID = "",
    selectRunName = "",
    preselRow = NULL
  )
  
  # list of sub-modules
  sub_modules <- list()
  
  #label
  labelrun <- "Run"
  if (batch) {
    labelrun <- "Batch"
  }
  
  # Modal for RunID selection --------------------------------------------------
  
  # > Modal Panel
  RunsList <- modalDialog(
    easyClose = TRUE,
    size = "l",
    flamingoTableUI(ns("tableInboxpanel")),
    footer = tagList(
      flamingoButton(ns("abuttonselectRun"),
                     label = "Select Run", align = "left"),
      actionButton(ns("abuttonccancel"),
                   label = "Cancel", align = "right")
    )
  )
  
  # > open modal
  observeEvent(input$chooseRunID, {
    data <- getInboxData(dbSettings, userId())
    result$inbox <- data  %>%
      replaceWithIcons() %>% 
      filter(Status == StatusCompleted)
    showModal(RunsList)
  })
  
  # > modal content
  sub_modules$tableInboxpanel <- callModule(
    flamingoTable,
    id = "tableInboxpanel",
    data = reactive(result$inbox),
    selection = "single",
    escape = FALSE,
    scrollX = TRUE,
    filter = FALSE,
    rownames = FALSE,
    colnames =  c("Row Number" = 1),
    preselRow = reactive({result$preselRow}),
    maxrowsperpage = 10,
    logMessage = logMessage)
  
  # > row to select
  observeEvent(preselectedRunId(), {
    result$preselRow <- match(preselectedRunId(), result$inbox[, inbox.RunID])
  })
  
  
  # > select run ID
  observeEvent(sub_modules$tableInboxpanel$rows_selected(), ignoreNULL = FALSE, {
    currid <- ""
    currName <- ""
    if (!is.null(sub_modules$tableInboxpanel$rows_selected())) {
      currid <- result$inbox[sub_modules$tableInboxpanel$rows_selected(),inbox.RunID]
      currName <- result$inbox[sub_modules$tableInboxpanel$rows_selected(),inbox.RunName]
    }
    result$selectRunID <- ifelse(is.null(currid) | is.na(currid), "", currid)
    result$selectRunName <-  ifelse(is.null(currName) | is.na(currName), "", currName)
  })
  
  # > close modal
  observeEvent(input$abuttonccancel, {
    removeModal()
  })
  
  observeEvent(input$abuttonselectRun, {
    if (!is.null(sub_modules$tableInboxpanel$rows_selected())) {
      result$preselRow <- sub_modules$tableInboxpanel$rows_selected()
    }
    removeModal()
  })
  
  output$selectRunInfo1 <- renderText({
    if (result$selectRunID == "") {
      info <- paste0("Select ", labelrun, ":   ")
    } else {
      info <- paste0('Selected ', labelrun, ': ')
    }
    info
  })
  
  output$selectRunInfo2 <- renderText({
    if (result$selectRunID == "") {
      info <- " ... "
    } else {
      info <- paste0(result$selectRunID, ' "' ,result$selectRunName, '"  ')
    }
    info
  })
  
  # Module Outout --------------------------------------------------------------
  selectRunID <- reactive({ifelse(is.null(result$selectRunID) | is.na(result$selectRunID), "", result$selectRunID)})
  
  moduleOutput <- c(
    list(
      selectRunID = reactive(selectRunID())
    )
  )
  
}

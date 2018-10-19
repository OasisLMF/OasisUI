# Summary Module ---------------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' @title summary_ui
#' Run Summary UI
#' @rdname summaryUI
#' @description Summary elements of a Run
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom shinyWidgets panel
#' @importFrom bsplus bs_embed_tooltip
#' @export
summaryUI <- function(id) {
  
  ns <- NS(id)
  
  flamingoPanel(
    id = ns("flamingoPanelSummaryTable"),
    collapsible = FALSE,
    heading = "Summary Table",
    panelSummaryTableModuleUI(ns( "panelSummaryTableModule"))
  )
  
}

# UI Functions -----------------------------------------------------------------

#' @title panelSummaryTableModuleUI
#' @rdname panelSummaryTableModuleUI
#' @inheritParams flamingoModuleUI
#' @importFrom DT DTOutput
#' @export
panelSummaryTableModuleUI <-  function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("outputsummarytable"))
  )
}

# Server -----------------------------------------------------------------------

#' @title summary_server
#' Run Summary Server
#' @rdname summary
#' @description Summary elements of a Run
#' @inheritParams flamingoModule
#' @return list of tags
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDT datatable
#' @importFrom dplyr mutate select contains filter
#' @export
#' @export
summary <- function(input, output, session, dbSettings,
                    apiSettings, userId,
                    selectRunID, active, logMessage = message) {
  
  ns <- session$ns
  
  # list of sub-modules
  sub_modules <- list()
  
  # collapse panel
  observeEvent(input$abuttonhidesummarytable, {
    num <- input$abuttonhidesummarytable
    if ((num %% 2) != 0 ) {
      updateActionButton(session = session, inputId = "abuttonhidesummarytable", label = NULL, icon = icon("expand"))
      hide("outputsummarytable")
    } else {
      updateActionButton(session = session, inputId = "abuttonhidesummarytable", label = NULL, icon = icon("minus"))
      show("outputsummarytable")
    }
  })
  
  # Summary table
  observeEvent(selectRunID(), ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (!is.null(selectRunID()) && !is.na(selectRunID())) {
      sub_modules$panelSummaryTableModule <- callModule(
        panelSummaryTableModule,
        id = "panelSummaryTableModule",
        selectRunID = reactive(selectRunID()),
        dbSettings = dbSettings,
        apiSettings = apiSettings,
        userId = userId,
        logMessage = logMessage)
    }
  })
  
}

# panelSummaryTableModule Module -----------------------
#' Module for Summary Table Panel
#' @rdname panelSummaryTableModule
#' @description Server logic to show the summary table output
#' @inheritParams flamingoModule
#' @param selectRunID selected runID
#' @return null
#' @importFrom DT renderDT datatable
#' @export
panelSummaryTableModule <- function(input, output, session, dbSettings,
                                    apiSettings, userId, logMessage = message, selectRunID ) {
  
  ns <- session$ns
  
  result <- reactiveValues(
    selectRunID = NULL,
    outputSummaryData = NULL
  )
  
  observe({
    result$selectRunID <- selectRunID()
  })
  
  observe({
    output$outputsummarytable <- renderDT({
      outputSummaryData <- executeDbQuery(dbSettings,
                                          paste("exec getOutputSummary", result$selectRunID))
      if (!is.null(outputSummaryData)) {
        datatable(
          outputSummaryData,
          class = "flamingo-table display",
          rownames = TRUE,
          selection = "none",
          colnames = c('Row Number' = 1),
          options = .getPRTableOptions()
        )
      } else {
        datatable(
          data.frame(content = "nothing to show"),
          class = "flamingo-table display",
          rownames = FALSE,
          selection = "none",
          colnames = c('Row Number' = 1),
          options = .getPRTableOptions()
        )
      }
      
    })
    
    
  })
  # Helper functions --------------------------------------------------------
  
  #table settings for pr tab: returns option list for datatable
  .getPRTableOptions <- function() {
    options <- list(
      search = list(caseInsensitive = TRUE),
      processing = 0,
      scrollX = TRUE,
      pageLength = 10,
      columnDefs = list(list(visible = FALSE, targets = 0)))
    return(options)
  }
  
  # Module Output -----------------------
  invisible()
}

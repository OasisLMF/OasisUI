# Summary Module ---------------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' @title summarytab_ui
#' Run Summary UI
#' @rdname summarytabUI
#' @description Summary elements of a Run
#' @inheritParams flamingoModuleUI
#' @return list of tags
#' @importFrom shinyWidgets panel
#' @importFrom bsplus bs_embed_tooltip
#' @export
summarytabUI <- function(id) {
  
  ns <- NS(id)
  
  flamingoPanel(
    id = ns("flamingoPanelSummaryTable"),
    collapsible = FALSE,
    heading =  tagAppendChildren(
      h4("Summary Table")
    ),
    fluidRow(
      column(6,
             h5("Inputs"),
             flamingoTableUI(ns("summaryInputTable"))
             ),
      column(6,
             h5("Parameters"),
             flamingoTableUI(ns("summaryParamsTable"))
      )
    ),
    fluidRow(
      column(6,
             h5("Output"),
             flamingoTableUI(ns("summaryOutputTable"))
      ),
      column(6,
             plotlyOutput(ns("summaryOutputPlot"))
      )
    )
   
        
  )
  
}

# Server -----------------------------------------------------------------------

#' @title summarytab_server
#' Run Summary Server
#' @rdname summarytab
#' @description Summary elements of a Run
#' @inheritParams flamingoModule
#' @return list of tags
#' @importFrom shinyjs show hide enable disable hidden
#' @importFrom DT renderDT datatable
#' @importFrom dplyr mutate select contains filter
#' @export
#' @export
summarytab <- function(input, output, session, dbSettings,
                       apiSettings, userId,
                       selectRunID, active, logMessage = message) {
  
  ns <- session$ns
  
  # Reactive Values & Params ---------------------------------------------------
  result <- reactiveValues(
    inputSummaryData = NULL,
    paramSummaryData = NULL,
    outputSummaryData = NULL
  )
  
  # list of sub-modules
  sub_modules <- list()
  
  # strings to filer
  outputStrg <- c("portfolio")
  paramStrg <- c("treshold|number|peril|set")
  
  # Extract Summary Data -------------------------------------------------------
  observeEvent(selectRunID(), {
    SummaryData <- executeDbQuery(dbSettings,
                                  paste("exec getOutputSummary", selectRunID()))
    if (!is.null(SummaryData)) {
    result$inputSummaryData <- SummaryData %>% 
      filter(!grepl(paste0(outputStrg, "|", paramStrg), SummaryType)) %>%
      as.data.frame()
    result$paramSummaryData <- SummaryData %>% 
      filter(grepl(paramStrg, SummaryType)) %>%
      as.data.frame()
    result$outputSummaryData <- SummaryData %>% 
      filter(grepl(outputStrg, SummaryType)) %>%
      as.data.frame()
    }
  })
  
  # Summary Input tables
  observeEvent(result$inputSummaryData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    sub_modules$summaryTable <- callModule(
      flamingoTable,
      id = "summaryInputTable",
      data = reactive(result$inputSummaryData),
      selection = "none",
      escape = TRUE,
      scrollX = FALSE,
      filter = FALSE,
      rownames = FALSE,
      colnames = FALSE,
      maxrowsperpage = 10,
      logMessage = logMessage)
  })
  
  # Summary Params tables
  observeEvent(result$paramSummaryData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    sub_modules$summaryTable <- callModule(
      flamingoTable,
      id = "summaryParamsTable",
      data = reactive(result$paramSummaryData),
      selection = "none",
      escape = TRUE,
      scrollX = FALSE,
      filter = FALSE,
      rownames = FALSE,
      colnames = FALSE,
      maxrowsperpage = 10,
      logMessage = logMessage)
  })
  
  # Summary Output tables
  observeEvent(result$outputSummaryData, ignoreNULL = FALSE, ignoreInit = TRUE, {
    sub_modules$summaryTable <- callModule(
      flamingoTable,
      id = "summaryOutputTable",
      data = reactive(result$outputSummaryData),
      selection = "none",
      escape = TRUE,
      scrollX = FALSE,
      rownames = FALSE,
      colnames = FALSE,
      maxrowsperpage = 10,
      logMessage = logMessage)
  })
  
}

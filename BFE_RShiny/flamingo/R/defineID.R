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
         # selectInput(inputId =  ns(paste0("selectRunID")), label = labelrun, choices = "", selected = NULL) %>%
         #   bs_embed_tooltip(title = browse_programmes$selectRunID, placement = "right")
         uiOutput(ns("selectRunInfo")),
         actionButton(ns(paste0("chooseRunID")), label = NULL, icon = icon("list-alt"), 
                                style = " color: black;
                                          background-color: white;
                                          padding: 0px;
                                          font-size: 24px;
                                          background-image: none;
                                          border: none;
                                ") %>%
           bs_embed_tooltip(title = browse_programmes$selectRunID, placement = "right")
         )
  
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
                     dbSettings, userId, 
                     runIdList = reactive(c(-1)),
                     preselectedRunId = reactive(-1),
                     logMessage = message) {
  
  ns <- session$ns
  
  # Reactive Values and parameters ---------------------------------------------
  result <- reactiveValues(
    inbox = NULL,
    selectRunID = "",
    selectRunName = ""
  )
  
  # list of sub-modules
  sub_modules <- list()

  # Update RunID----------------------------------------------------------------
  
  # observeEvent(preselectedRunId(), {
  #   index <- match(c(preselectedRunId()), runIdList()$RunID)
  #   if (!is.null(index) & !is.na(index)) {
  #     updateSelectInput(session, inputId = "selectRunID", choices = runIdList()$RunID, selected = runIdList()$RunID[index])
  #   }
  # })
  
  # observeEvent(input[["selectRunID"]], ignoreInit = TRUE, {
  #   result$selectRunID <- input[["selectRunID"]]
  # })
  
  observeEvent(input$chooseRunID, {
    data <- getInboxData(dbSettings, userId())
    result$inbox <- data  %>%
      replaceWithIcons() %>% 
      filter(Status == StatusCompleted)
    showModal(RunsList)
    sub_modules$tableInbox <- callModule(
      flamingoTable,
      id = "tableInbox",
      data = reactive(data),
      selection = "single",
      escape = FALSE,
      scrollX = TRUE,
      filter = FALSE,
      rownames = TRUE,
      colnames =  c("Row Number" = 1),
      maxrowsperpage = 10,
      logMessage = logMessage)
    
    print(names(result$inbox))
  })
  
  observeEvent(input$selectRun, {
    if (!is.null(result$inbox)) {
      result$selectRunID <- result$inbox[input$tableInbox_selected_rows,1]
      result$selectRunName <- result$inbox[input$tableInbox_selected_rows,2]
    }

  })
  
  # Modal Panel
  RunsList <- modalDialog(
    easyClose = TRUE,
    size = "l",
    flamingoTableUI(ns("tableInbox")),
    flamingoButton(ns("selectRun"), label = "Select Run")
  )
  
  # text infos on Run ID
  output$selectRunInfo <- renderUI(h5(paste0("Selected Run ", result$selectRunID, " " ,result$selectRunName)))
  
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

# define One ID module ---------------------------------------------------------
# UI ---------------------------------------------------------------------------

#' defineIDUI
#'
#' @rdname defineID
#'
#' @description UI/View for defining one analysis ID
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

  labelana <- "Ana ID"
  if (batch) {
    labelana <- "Batch ID"
  }

  column(w,
         actionButton(ns(paste0("chooseAnaID")), label = NULL, icon = icon("list-alt"),
                      style = " color: rgb(71, 73, 73);
                               background-color: white;
                               padding: 0px;
                               font-size: 24px;
                               background-image: none;
                               border: none;
                                ") %>%
           bs_embed_tooltip(title = browse_programmes$selectAnaID, placement = "right"),
         div(textOutput(ns("selectAnaInfo1"), inline = TRUE),
               style = "font-weight:bold; font-color: #2d2d2d;
                        display:inline;
                        padding:10px; margin: 5px; "),
         div(textOutput(ns("selectAnaInfo2"), inline = TRUE),
             style = "display:inline;"
                      # font-weight:bold; font-color: #2d2d2d;
                      # padding:10px; margin: 5px; border-style: solid;"
             ),
         style = "display:inline;")

}

# Server -----------------------------------------------------------------------

#' defineID
#'
#' @rdname defineID
#'
#' @description Server logic for defining one analysis ID.
#'
#' @template return-outputNavigation
#' @template params-module
#' @template params-flamingo-module
#'
#' @param preselAnaId reactive string expression for reselected analysis id from \link{landingpage}.
#' @param processRunId reactive string expression for reselected run id from \link{defineProgramme}.
#'
#' @param batch Flag indicating if it is a batch or a simple analysis.
#'
#' @return selectAnaID reactive for anaID selected.
#'
#' @export
defineID <- function(input, output, session,
                     dbSettings, user,
                     preselAnaId = reactive(-1),
                     processRunId = reactive(-1),
                     batch = FALSE,
                     logMessage = message) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------
  result <- reactiveValues(
    inbox = NULL,
    selectAnaID = "",
    selectAnaName = "",
    LProw = NULL,
    PRrow = NULL,
    preselRow = NULL
  )

  # list of sub-modules
  sub_modules <- list()

  #label
  labelana <- "Ana"
  if (batch) {
    labelana <- "Batch"
  }

  # Modal for AnaID selection --------------------------------------------------

  # > Modal Panel
  AnaList <- modalDialog(
    easyClose = TRUE,
    size = "l",
    flamingoTableUI(ns("tableInboxpanel")),
    footer = tagList(
      flamingoButton(ns("abuttonselectAna"),
                     label = "Select Analysis", align = "left"),
      actionButton(ns("abuttonccancel"),
                   label = "Cancel", align = "right")
    )
  )

  # > open modal
  observeEvent(input$chooseAnaID, {
    data <- getInboxData(dbSettings, user())
    result$inbox <- data  %>%
      replaceWithIcons() %>%
      filter(Status == StatusCompleted)
    showModal(AnaList)
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

  #Find row of anaid preselected in landing page
  observeEvent({
    preselAnaId()},{
      idx <- which(result$inbox[, inbox.AnaID] == preselAnaId())
      status <- result$inbox[idx,  inbox.Status]

      if (length(idx) > 0 && status == StatusCompleted){
        result$LProw <- idx
      }
    })

  #Find row of anaid preselected in model analysis server step 3
  observeEvent({
    processRunId()},{
      idx <- which(result$inbox[, inbox.AnaID] ==  processRunId())
      status <- result$inbox[idx,  inbox.Status]

      if (length(idx) > 0 && status == StatusCompleted){
        result$PRrow <- idx
      }
    })

  observeEvent({
    result$LProw
    result$PRrow
  },ignoreNULL = FALSE, {
    if (!is.null(result$LProw)) {
      result$preselRow <- result$LProw
    } else if (!is.null(result$PRrow)) {
      result$preselRow <- result$PRrow
    } else {
      result$preselRow <- 1
    }
  })


  # > select analysis ID
  observeEvent(sub_modules$tableInboxpanel$rows_selected(), ignoreNULL = FALSE, {
    currid <- ""
    currName <- ""
    if (!is.null(sub_modules$tableInboxpanel$rows_selected())) {
      currid <- result$inbox[sub_modules$tableInboxpanel$rows_selected(),inbox.AnaID]
      currName <- result$inbox[sub_modules$tableInboxpanel$rows_selected(),inbox.AnaName]
    }
    result$selectAnaID <- ifelse(is.null(currid) | is.na(currid), "", currid)
    result$selectAnaName <-  ifelse(is.null(currName) | is.na(currName), "", currName)
  })

  # > close modal
  observeEvent(input$abuttonccancel, {
    removeModal()
  })

  observeEvent(input$abuttonselectAna, {
    if (!is.null(sub_modules$tableInboxpanel$rows_selected())) {
      result$preselRow <- sub_modules$tableInboxpanel$rows_selected()
    }
    removeModal()
  })

  output$selectAnaInfo1 <- renderText({
    if (result$selectAnaID == "") {
      info <- paste0("Select ", labelana, ":   ")
    } else {
      info <- paste0('Selected ', labelana, ': ')
    }
    info
  })

  output$selectAnaInfo2 <- renderText({
    if (result$selectAnaID == "") {
      info <- " "
    } else {
      info <- paste0(result$selectAnaID, ' "' ,result$selectAnaName, '"  ')
    }
    info
  })

  # Module Outout --------------------------------------------------------------
  selectAnaID <- reactive({ifelse(is.null(result$selectAnaID) | is.na(result$selectAnaID), "", result$selectAnaID)})

  moduleOutput <- c(
    list(
      selectAnaID = reactive(selectAnaID())
    )
  )

}

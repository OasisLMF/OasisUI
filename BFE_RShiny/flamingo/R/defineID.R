# define One ID module ---------------------------------------------------------
# UI ---------------------------------------------------------------------------

#' defineIDUI
#'
#' @rdname defineID
#'
#' @description UI/View for defining one analysis ID
#'
#' @param w width of the coulmn.
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
           bs_embed_tooltip(title = dashboard$selectAnaID, placement = "right"),
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
#' @template params-logMessage
#'
#' @param preselAnaId reactive string expression for reselected analysis id from \link{landingPage}.
#' @param anaID reactive string expression for reselected run id from \link{step3_configureOutput}.
#'
#' @param batch Flag indicating if it is a batch or a simple analysis.
#'
#' @return selectAnaID reactive for anaID selected.
#' 
#' @importFrom dplyr sym
#' @importFrom dplyr filter
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#'
#' @export
defineID <- function(input, output, session,
                     preselAnaId = reactive(-1),
                     anaID = reactive(-1),
                     batch = FALSE,
                     logMessage = message) {
  
  ns <- session$ns
  
  # Reactive Values and parameters ---------------------------------------------
  result <- reactiveValues(
    tbl_analysesData = NULL,
    selectAnaID = "",
    selectAnaName = "",
    selectportfolioID = "",
    preselRow = NULL
  )
  
  # list of sub-modules
  sub_modules <- list()
  
  #label
  labelana <- "Analysis"
  if (batch) {
    labelana <- "Batch Analysis"
  }
  
  # Modal for AnaID selection --------------------------------------------------
  
  # > Modal Panel
  AnaList <- modalDialog(
    easyClose = TRUE,
    size = "l",
    flamingoTableUI(ns("flamingo_analyses")),
    footer = tagList(
      flamingoButton(ns("abuttonselectAna"),
                     label = "Select Analysis", align = "left") %>%
        bs_embed_tooltip(title = dashboard$abuttonselectAna, placement = "right"),
      actionButton(ns("abuttoncancel"),
                   label = "Cancel", align = "right")
    )
  )
  
  # > update lsit of analyses
  observeEvent({
    input$chooseAnaID
    preselAnaId()
    anaID()}, ignoreInit = TRUE, {
      tbl_analysesData  <- return_tbl_analysesData() 
      if (!is.null(tbl_analysesData) && nrow(tbl_analysesData) > 0) {
        result$tbl_analysesData <- tbl_analysesData  %>%
          filter(!! sym(tbl_analysesDataNames$status) == Status$Completed)
      }
    })
  
  # > open modal
  observeEvent(
    input$chooseAnaID, {
      showModal(AnaList)
    })
  
  
  # > modal content
  sub_modules$flamingo_analyses <- callModule(
    flamingoTable,
    id = "flamingo_analyses",
    data = reactive(result$tbl_analysesData),
    selection = "single",
    escape = FALSE,
    scrollX = TRUE,
    filter = FALSE,
    rownames = FALSE,
    colnames =  c("row number" = 1),
    preselRow = reactive({result$preselRow}),
    maxrowsperpage = 10,
    logMessage = logMessage)
  
  # > enable disable button
  observeEvent(sub_modules$flamingo_analyses$rows_selected(), ignoreNULL = FALSE, {
    if (is.null(sub_modules$flamingo_analyses$rows_selected())) {
      disable("abuttonselectAna") 
    } else {
      enable("abuttonselectAna")
    }
  })
  
  #Find row of anaid preselected in landing page
  observeEvent({
    preselAnaId()},{
      idx <- which(result$tbl_analysesData[,tbl_analysesDataNames$id] == preselAnaId())
      if (length(idx) > 0 && !isTRUE(all.equal(result$preselRow, idx)) && !isTRUE(all.equal(sub_modules$flamingo_analyses$rows_selected(), idx))) {
        result$preselRow <- idx
      }
    })
  
  #Find row of anaid preselected in model analysis server step 3
  observeEvent({
    anaID()},{
      idx <- which(result$tbl_analysesData[,tbl_analysesDataNames$id] == anaID())
      if (length(idx) > 0 && !isTRUE(all.equal(result$preselRow, idx)) && !isTRUE(all.equal(sub_modules$flamingo_analyses$rows_selected(), idx))) {
        result$preselRow <- idx
      }
    })
  
  
  # > select analysis ID
  observeEvent(result$preselRow, {
    .downloadOutput(idx = result$preselRow)
  })
  
  
  observeEvent(input$abuttonselectAna, {
    .downloadOutput(idx = sub_modules$flamingo_analyses$rows_selected())
    removeModal()
  })
  
  # > Enable/disable select button
  observeEvent(sub_modules$flamingo_analyses$rows_selected(), ignoreNULL = FALSE, {
    if (!is.null(sub_modules$flamingo_analyses$rows_selected())) {
      enable("abuttonselectAna")
    } else {
      disable("abuttonselectAna")
    }
  })
  
  # > close modal
  observeEvent(input$abuttoncancel, {
    removeModal()
  })
  
  # > ifo selected analysis
  output$selectAnaInfo1 <- renderText({
    paste0("Selected ", labelana, ":   ")
  })
  
  output$selectAnaInfo2 <- renderText({
    if (result$selectAnaID == "") {
      info <- '" - "'
    } else {
      info <- paste0(result$selectAnaID, ' "' ,result$selectAnaName, '"  ') 
    }
    info
  })
  
  # Help functions -------------------------------------------------------------
  .downloadOutput <- function(idx) {
    currid <- ""
    currName <- ""
    currpfId <- ""
    if (!is.null(idx)) {
      currid <- result$tbl_analysesData[idx,tbl_analysesDataNames$id]
      currName <- result$tbl_analysesData[idx,tbl_analysesDataNames$name]
      currpfId <- result$tbl_analysesData[idx,tbl_analysesDataNames$portfolio]
    }
    result$selectAnaID <- ifelse(is.null(currid) | is.na(currid), "", currid)
    result$selectAnaName <-  ifelse(is.null(currName) | is.na(currName), "", currName)
    result$selectportfolioID <- ifelse(is.null(currpfId) | is.na(currpfId), "", currpfId)
    logMessage("Extract output files")
    api_get_analyses_output_file(result$selectAnaID)
  }
  
  # Module Outout --------------------------------------------------------------
  
  moduleOutput <- c(
    list(
      selectAnaID = reactive({result$selectAnaID}),
      selectPortfolioID = reactive({result$selectportfolioID})
    )
  )
  
}

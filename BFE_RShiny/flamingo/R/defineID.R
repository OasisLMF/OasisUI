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
#' @template params-active
#'
#' @param preselAnaId reactive string expression for reselected analysis id from \link{landingPage}.
#' @param anaID reactive string expression for reselected run id from \link{step3_configureOutput}.
#'
#' @param batch Flag indicating if it is a batch or a simple analysis.
#'
#' @return selectAnaID reactive for the id the selected analysis.
#' @return selectPortfolioID reactive for portfolio id associated with the selected analysis.
#'
#' @importFrom dplyr sym
#' @importFrom dplyr filter
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#'
#' @export
defineID <- function(input, output, session,
                     preselAnaId = reactive(NULL),
                     anaID = reactive(NULL),
                     batch = FALSE,
                     active = reactive(TRUE)) {

  ns <- session$ns

  # Reactive Values and parameters ---------------------------------------------
  result <- reactiveValues(
    tbl_analysesData = NULL,
    selectAnaID = NULL,
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

  # Table and AnaID selection --------------------------------------------------

  # > find row of anaid preselected in landing page
  observeEvent({preselAnaId()}, {
      .reload_tbl_analysesData()
      logMessage(paste0("Updating preselected row because preselAnaId() changed to ", preselAnaId()))
      .preselectAna(preselAnaId())
    })

  # > find row of anaid preselected in model analysis server step 3
  observeEvent({anaID()}, {
      .reload_tbl_analysesData()
      logMessage(paste0("Updating preselected row because anaID() changed to ", anaID()))
      .preselectAna(anaID())
    })

  # > open modal
  observeEvent(input$chooseAnaID, ignoreInit = TRUE, {
      .reload_tbl_analysesData()
      showModal(AnaList)
    })

  # Modal Panel ----------------------------------------------------------------

  # > modal content
  sub_modules$flamingo_analyses <- callModule(
    flamingoTable,
    id = "flamingo_analyses",
    data = reactive({result$tbl_analysesData}),
    selection = "single",
    escape = FALSE,
    filter = FALSE,
    rownames = FALSE,
    preselRow = reactive({result$preselRow}),
    maxrowsperpage = 10)

  # > Modal Dialogue
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

  # > close modal
  observeEvent(input$abuttoncancel, {
    removeModal()
  })

  # Enable / Disable buttons ---------------------------------------------------

  # > enable disable button
  observeEvent(sub_modules$flamingo_analyses$rows_selected(), ignoreNULL = FALSE, {
    if (is.null(sub_modules$flamingo_analyses$rows_selected())) {
      disable("abuttonselectAna")
    } else {
      enable("abuttonselectAna")
    }
  })

  # > Enable/disable select button
  observeEvent(sub_modules$flamingo_analyses$rows_selected(), ignoreNULL = FALSE, {
    if (!is.null(sub_modules$flamingo_analyses$rows_selected())) {
      enable("abuttonselectAna")
    } else {
      disable("abuttonselectAna")
    }
  })

  # Download data --------------------------------------------------------------

  # > download data for preselected analysis
  observeEvent(result$preselRow, {
    if (!is.null( result$preselRow)) {
      withModalSpinner(
        .downloadOutput(idx = result$preselRow),
        "Loading...",
        size = "s"
      )
    }
  })

  # > download data after clicking on button to select analysis
  observeEvent(input$abuttonselectAna, {
    removeModal()
    withModalSpinner(
      .downloadOutput(idx = sub_modules$flamingo_analyses$rows_selected()),
      "Loading...",
      size = "s"
    )
  })

  # Display analysis infos -----------------------------------------------------

  output$selectAnaInfo1 <- renderText({
    paste0("Selected ", labelana, ":   ")
  })

  output$selectAnaInfo2 <- renderText({
    if (is.null(result$selectAnaID)) {
      info <- '" - "'
    } else {
      info <- paste0(result$selectAnaID, ' "' , result$selectAnaName, '"  ')
    }
    info
  })

  # Help functions -------------------------------------------------------------
  .downloadOutput <- function(idx) {
    currid <- NULL
    currName <- ""
    currpfId <- ""
    if (!is.null(idx)) {
      currid <- result$tbl_analysesData[idx,tbl_analysesDataNames$id]
      currName <- result$tbl_analysesData[idx, tbl_analysesDataNames$name]
      currpfId <- result$tbl_analysesData[idx, tbl_analysesDataNames$portfolio]
    }
    result$selectAnaID <- ifelse(is.null(currid), NULL, currid)
    result$selectAnaName <-  ifelse(is.null(currName) || is.na(currName), "", currName)
    result$selectportfolioID <- ifelse(is.null(currpfId) || is.na(currpfId), "", currpfId)
    # logMessage("Extract output files")
    # session$userData$data_hub$get_ana_outputs_data_list(result$selectAnaID, oasisapi = session$userData$oasisapi)
  }


  .reload_tbl_analysesData <- function(){
    tbl_analysesData <- return_tbl_analysesData(oasisapi =  session$userData$oasisapi)
    if (!is.null(tbl_analysesData) && nrow(tbl_analysesData) > 0) {
      result$tbl_analysesData <- tbl_analysesData  %>%
        filter(!! sym(tbl_analysesDataNames$status) == Status$Completed)
    } else {
      result$tbl_analysesData <- NULL
    }
  }

  .preselectAna <- function(preselAna) {
    if (!is.null(result$tbl_analysesData) && nrow(result$tbl_analysesData) > 0 ) {
      idx <- which(result$tbl_analysesData[,tbl_analysesDataNames$id] == preselAna)
      if (length(idx) > 0 &&
          !isTRUE(all.equal(result$preselRow, idx)) &&
          !isTRUE(all.equal(sub_modules$flamingo_analyses$rows_selected(), idx))) {
        result$preselRow <- idx
      }
    }
  }

  # Module Outout --------------------------------------------------------------

  moduleOutput <- c(
    list(
      selectAnaID = reactive({result$selectAnaID}),
      selectPortfolioID = reactive({result$selectportfolioID})
    )
  )

}

# uploaded inputs Module ----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' generatedinputsUI
#' @rdname generatedinputs
#'
#' @description UI/View for uploaded inputs of an analysis.
#'
#' @return List of tags.
#'
#' @export
generatedinputsUI <- function(id) {

  ns <- NS(id)

  tagList(
    flamingoPanel(
      selection = list(mode = 'none'),
      escape = FALSE,
      scrollX = TRUE,
      filter = "none",
      rownames = TRUE,
      colnames = c('row number' = 1),
      id = ns("panel_analysisdetails"),
      flamingoRefreshButton(ns("abuttongeneratedrefresh")),
      ViewFilesInTableUI(id  = ns("ViewIGFiles"), includechkbox = TRUE)
    )
  )
}

# Server -----------------------------------------------------------------------

#' generatedinputs
#'
#' @rdname generatedinputs
#'
#' @description  Server logic for uploaded inputs of an analysis.
#'
#' @param analysisID selected analysis ID.
#' @param param AnalysisID
#'
#' @export
generatedinputs <- function(input,
                            output,
                            session,
                            analysisID) {

  ns <- session$ns

  result <- reactiveValues(
    data = NULL,
    dt_generated = NULL
  )

  #reload input generated table
  .reloadGeneratediInputs <- function(){
    logMessage(".reloadGeneratediInputs called")
    if (!is.null(analysisID()) && analysisID() != "") {
      result$data <- return_analyses_input_file_wicons_df(analysisID())
    } else {
      result$data <-  NULL
    }
    result$data
  }

  result$dt_generated <- reactive({
    .reloadGeneratediInputs()
  })

  sub_modules <- list()
  sub_modules$ViewIGFiles <- callModule(
    ViewFilesInTable,
    id = "ViewIGFiles",
    tbl_filesListData = result$dt_generated,
    param = analysisID,
    file_column = "files",
    folderpath = "_inputs/",
    includechkbox = TRUE)

  onclick("abuttongeneratedrefresh", {
    .reloadGeneratediInputs()
  })
}

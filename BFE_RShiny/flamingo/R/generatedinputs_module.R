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
#' @param file_column Column in the file.
#' @param folderpath Path to folder.
#'
#' @export
generatedinputs <- function(input,
                            output,
                            session,
                            analysisID,
                            param,
                            file_column,
                            folderpath) {

  ns <- session$ns

  sub_modules <- list()

  dt_anaIG <- reactive({
    if (!is.null(analysisID()) && analysisID() != "") {
      tbl_anaIG <- return_analyses_input_file_wicons_df(analysisID())
    } else {
      tbl_anaIG <-  NULL
    }
    tbl_anaIG
  })

  sub_modules$ViewIGFiles <- callModule(
    ViewFilesInTable,
    id = "ViewIGFiles",
    tbl_filesListData = dt_anaIG,
    param = param,
    file_column = file_column,
    folderpath = folderpath,
    includechkbox = TRUE)

  sub_modules

  #reload input generated table
  .reloadAnaIG <- function(){
    logMessage(".reloadAnaIG called")
    if (!is.null(analysisID()) && analysisID() != "") {
      tbl_anaIG <- return_analyses_input_file_wicons_df(analysisID())
    } else {
      tbl_anaIG <-  NULL
    }
    tbl_anaIG
  }

  onclick("abuttongeneratedrefresh", {
    .reloadAnaIG()
  })
}

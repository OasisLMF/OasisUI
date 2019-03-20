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
#' @param tbl_filesListData Table containing the list of files.
#' @param param AnalysisID
#' @param file_column Column in the file.
#' @param folderpath Path to folder.
#'
#' @export
generatedinputs <- function(input,
                            output,
                            session,
                            tbl_filesListData,
                            param,
                            file_column,
                            folderpath) {

  ns <- session$ns

  sub_modules <- list()

  sub_modules$ViewIGFiles <- callModule(
    ViewFilesInTable,
    id = "ViewIGFiles",
    tbl_filesListData = tbl_filesListData,
    param = param,
    file_column = file_column,
    folderpath = folderpath,
    includechkbox = TRUE)

  sub_modules
}

# output files Module ----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' outputfilesUI
#' @rdname outputfiles
#'
#' @description UI/View for output files of an analysis.
#'
#' @return List of tags.
#'
#' @export
outputfilesUI <- function(id) {

  ns <- NS(id)

  tagList(

    flamingoPanel(
      id = ns("flamingoPanelViewOutputFiles"),
      collapsible = TRUE,
      show = TRUE,
      heading = "Output files table",
      ViewFilesInTableUI(id  = ns("ViewOutputFiles"), includechkbox = TRUE)
    ),

    flamingoPanel(
      id = ns("flamingoPanelViewInputFiles"),
      collapsible = TRUE,
      show = FALSE,
      heading = "Input files table",
      anainputsUI(id  = ns("anainputs"))
    )
  )
}


# Server -----------------------------------------------------------------------

#' outputfiles
#'
#' @rdname outputfiles
#'
#' @description  Server logic for output files of an analysis.
#'
#' @template params-module
#' @template params-logMessage
#' @template params-active
#' @param tbl_filesListDataana tbl of output files to view
#' @param anaId id of analysis
#' @param portfolioId id of portfolio associated to the analysis
#'
#' @export
outputfiles <- function(input, output, session,
                        tbl_filesListDataana = reactive(NULL),
                        anaId = reactive(""),
                        portfolioId = reactive(""),
                        active, counter, logMessage = message) {

  ns <- session$ns

  # list of sub-modules
  sub_modules <- list()

  sub_modules$ViewOutputFiles <- callModule(
    ViewFilesInTable,
    id = "ViewOutputFiles",
    tbl_filesListData = tbl_filesListDataana,
    param = anaId,
    logMessage = logMessage,
    file_column = "files",
    includechkbox = TRUE)

  sub_modules$anainputs <- callModule(
    anainputs,
    id = "anainputs",
    analysisID = anaId,
    portfolioID = portfolioId,
    refresh_opt = FALSE,
    counter = counter,
    active = active
  )
}

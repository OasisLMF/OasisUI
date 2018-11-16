# output files Module ----------------------------------------------------------

# UI ---------------------------------------------------------------------------
#' outputfilesUI
#' @rdname outputfiles
#'
#' @description UI/View for output files of an analysis.
#'
#' @template params-module-ui
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
      heading = "Output Files Table",
      ViewFilesModuleUI(id  = ns("ViewOutputFilesModule"), includechkbox = TRUE)
    ),

    flamingoPanel(
      id = ns("flamingoPanelViewInputFiles"),
      collapsible = TRUE,
      show = FALSE,
      heading = "Input Files Table",
      ViewFilesModuleUI(id  = ns("ViewInputFilesModule"), includechkbox = TRUE)
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
#' @template params-flamingo-module
#'
#' @param filesListDatatoview dataframe of files to view
#'
#' @export
outputfiles <- function(input, output, session, dbSettings,
                        apiSettings,
                        filesListDatatoview, active, logMessage = message) {

  ns <- session$ns

  # list of sub-modules
  sub_modules <- list()

  sub_modules$ViewFilesModule <- callModule(
    ViewFilesModule,
    id = "ViewOutputFilesModule",
    filesListData =  filesListDatatoview,
    logMessage = logMessage,
    includechkbox = TRUE)

  sub_modules$ViewFilesModule <- callModule(
    ViewFilesModule,
    id = "ViewInputFilesModule",
    filesListData =  reactive(NULL),
    logMessage = logMessage,
    includechkbox = TRUE)
}
